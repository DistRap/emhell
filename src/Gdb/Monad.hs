module Gdb.Monad where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import System.Posix.Signals (installHandler, Handler(Catch), sigINT)

import Text.Printf            (printf)

import qualified Gdbmi.IO             as G
import qualified Gdbmi.Commands       as C
import qualified Gdbmi.Semantics      as S
import qualified Gdbmi.Representation as R

data GDBContext = GDBContext {
    contextGdbMI   :: G.Context
  , contextStops   :: TMVar [S.Stopped]
  , contextLog     :: TBQueue String
  }

type GdbT m = ReaderT GDBContext m
type GdbMonad a = GdbT IO a

defaultConfig :: G.Config
defaultConfig = G.Config ["gdb"] (Just "gdb.log")

armConfig :: G.Config
armConfig = G.Config ["arm-none-eabi-gdb"] (Just "gdb.log")

tcpConfig :: String -> Int -> G.Config
tcpConfig host port = G.ConfigTCP host port (Just "gdb-tcp.log")

runGdb :: (MonadIO m) => (GdbT m a) -> m (Either String a)
runGdb = runGdbConfig defaultConfig

runGdbConfig :: MonadIO m
             => G.Config
             -> GdbT m b
             -> m (Either String b)
runGdbConfig config act = withGDB config act

sigintHandler :: IO b -> IO b
sigintHandler ofWhat = do
    tid <- myThreadId
    bracket (installHandler sigINT (Catch $ throwTo tid UserInterrupt) Nothing)
        (\old -> installHandler sigINT old Nothing) $ pure $ ofWhat

-- | Set-up GDB connection and run `GdbT` application
withGDB :: MonadIO m => G.Config -> GdbT m b -> m (Either String b)
withGDB config act = do
  (stops, streamQ, notifQ, userQ) <- liftIO $ do
    stops <- atomically $ newEmptyTMVar
    (streamQ, notifQ, userQ) <- atomically $ (,,)
      <$> newTBQueue 1000
      <*> newTBQueue 1000
      <*> newTBQueue 1000
    return (stops, streamQ, notifQ, userQ)
  ctx <- liftIO $ G.setup config (callbackQueues stops streamQ notifQ)

  x <- flip runReaderT (GDBContext ctx stops userQ) act
  liftIO $ G.shutdown ctx
  return $ Right x

-- | `withGdb` variant that prints data from queues, installs `sigintHandler`
-- and prints all logs at the end
withGDB' :: MonadIO m => G.Config -> GdbT IO b -> m (Either String b)
withGDB' config act = liftIO $ do
  stops <- atomically $ newEmptyTMVar
  (streamQ, notifQ, userQ) <- atomically $ (,,)
    <$> newTBQueue 1000
    <*> newTBQueue 1000
    <*> newTBQueue 1000

  lock <- newMVar ()
  --- XXX: make this verbose mode configurable
  void $ async $ forever $ do
    stream <- atomically $ readTBQueue streamQ
    withMVar lock $ const $ pstream stream

  void $ async $ forever $ do
    q <- atomically $ readTBQueue userQ
    withMVar lock $ const $ putStrLn q

  ctx <- G.setup config (callbackQueues stops streamQ notifQ)

  x <- try
    $ sigintHandler
    $ flip runReaderT (GDBContext ctx stops userQ) act

  res <- case x of
    Left e -> return $ Left $ show (e :: SomeException)
    Right r -> return $ Right r

  G.shutdown ctx

  -- dump remaining data in streams
  (logs, _events) <- atomically $ (,) <$> flushTBQueue streamQ <*> flushTBQueue notifQ

  pstream $ concat logs
  -- we ignore async notifications (events) for now
  --print notif
  return res
  where
    pstream' (R.Stream _ s) = putStr s
    pstream = mapM_ pstream'

callbackQueues :: TMVar [S.Stopped] -> TBQueue [R.Stream] -> TBQueue [R.Notification] -> G.Callback
callbackQueues mv logs events = G.Callback
  (toQueue logs)
  (toQueue events)
  (Just (atomically . putTMVar mv))
  where
    toQueue q = atomically . writeTBQueue q

-- |Send GDB-MI command and fail if response differs from
-- expected response `rc`
command :: G.Context -> R.ResultClass -> R.Command -> IO [R.Result]
command ctx rc x = do
  resp <- G.send_command ctx x
  let msg = printf "command '%s' failed (%s): %s"
              (R.render_command x)
              (show (R.respClass resp))
              ((show . S.response_error . R.respResults) resp)
  when (R.respClass resp /= rc) (error msg)
  return (R.respResults resp)

-- |Send GDB-MI command and return raw response
commandRaw :: G.Context -> R.Command -> IO R.Response
commandRaw ctx x = do
  resp <- G.send_command ctx x
  return resp

-- |Send GDB command but don't fail if response differs from
-- expected response `rc`, print error message instead
cmdWarn :: (MonadIO m) => R.ResultClass -> R.Command -> GdbT m ()
cmdWarn rc x = do
  ctx <- fmap contextGdbMI ask
  resp <- liftIO $ commandRaw ctx x
  let msg = printf "command '%s' got unexpected response (%s): %s"
              (R.render_command x)
              (show (R.respClass resp))
              ((show . S.response_error . R.respResults) resp)
  when (R.respClass resp /= rc) (echo msg)

-- |Accept any response class
cmdAny :: (MonadIO m) => R.Command -> GdbT m ()
cmdAny x = do
  ctx <- fmap contextGdbMI ask
  void $ liftIO $ commandRaw ctx x


-- |Send GDB command expecting `rc` response
cmd :: (MonadIO m) => R.ResultClass -> R.Command -> GdbT m [R.Result]
cmd rc x = do
  ctx <- fmap contextGdbMI ask
  liftIO $ command ctx rc x

-- |Like `cmd` but discards result
cmd' :: (MonadIO m) => R.ResultClass -> R.Command -> GdbT m ()
cmd' rc x = void $ cmd rc x

-- |Run program loaded in GDB
run :: (MonadIO m) => GdbT m [R.Result]
run = do
  cmd R.RCRunning $ C.exec_run

-- |Continue execution (for example after breakpoint)
continue :: (MonadIO m) => GdbT m ()
continue = do
  cmd' R.RCRunning $ C.exec_continue

-- |Interrupt background execution of the target
interrupt :: (MonadIO m) => GdbT m ()
interrupt = do
  cmd' R.RCDone $ C.exec_interrupt (Left True) -- --all

-- |Wait for stop condition (e.g. breakpoint)
waitStop :: (MonadIO m) => GdbT m [S.Stopped]
waitStop = do
  tmvar <- fmap contextStops ask

  _flushedOldStops <- liftIO $ atomically $ do
    tryTakeTMVar tmvar

  stops <- liftIO $ atomically $ do
    takeTMVar tmvar
  return stops

-- |Did we stop due to breakpoint
isBreak :: S.Stopped -> Bool
isBreak = isBreakHit . S.stoppedReason

-- |Did we stop due to breakpoint
isBreakHit :: S.StopReason -> Bool
isBreakHit S.BreakpointHit{} = True
isBreakHit _ = False

-- |Wait till breakpoint is hit
waitBreak :: (MonadIO m) => GdbT m S.Stopped
waitBreak = do
  stops <- waitStop
  if any isBreak stops
    then return $ head $ filter isBreak stops -- return ()
    else waitBreak

-- |Perform action `act` when breakpoint is hit and continue
-- afterwards
onBreak :: (MonadIO m) => (S.Stopped -> GdbT m ()) -> GdbT m ()
onBreak act = do
  brk <- waitBreak
  act brk
  continue

-- XXX: more pretty
showStops :: (MonadIO m) => [S.Stopped] -> GdbT m ()
showStops = mapM_ showStop

showStop :: (MonadIO m, Show a) => a -> m ()
showStop s = liftIO $ print s -- show (stoppedReason s)

-- |Run raw GDB cli command
cli :: (MonadIO m) => String -> GdbT m ()
cli x = cmdAny $ C.cli_command x

echo :: (MonadIO m) => String -> GdbT m ()
echo msg = do
  logQ <- fmap contextLog ask
  liftIO $ atomically $ writeTBQueue logQ msg

-- |Send Ctrl-C to GDB
break :: (MonadIO m) => GdbT m ()
break = do
  ctx <- fmap contextGdbMI ask
  liftIO $ G.interrupt ctx

-- |Create new breakpoint
breakpoint :: (MonadIO m) => C.Location -> GdbT m ()
breakpoint loc = void $ cmd R.RCDone $
  C.break_insert False False False False False Nothing Nothing Nothing loc

-- |Like `p` (var printing)
eval :: (MonadIO m) => String -> GdbT m String
eval expr = do
  value' <- cmd R.RCDone $ C.data_evaluate_expression expr
  let (Just value) = S.response_data_evaluate_expression value'
  return value

readMem :: (MonadIO m, Show a, Num b) => a -> Int -> GdbT m (Maybe b)
readMem addr size = do
  res <- cmd R.RCDone $ C.data_read_memory_bytes Nothing (show addr) size
  return $ S.response_read_memory_bytes res

data Programmer =
    BMP String
  | BMPHosted String Int
  deriving (Eq, Show)

toTarget :: Programmer -> C.Medium
toTarget (BMP dev) = C.SerialDevice dev
toTarget (BMPHosted host port) = C.TcpHost host port

extRemote :: (MonadIO m) => Programmer -> GdbT m ()
extRemote prog = do
  cmd' R.RCDone $ C.gdb_set "mi-async on"
  _ <- cmd R.RCConnected $ C.target_select $ C.ExtendedRemote $ toTarget prog
  cli "monitor swdp_scan"
  cli "monitor connect_srs disable"
  cli "set mem inaccessible-by-default off"
  cmd' R.RCDone $ C.gdb_set "mem inaccessible-by-default off"
  _ <- cmd R.RCDone $ C.target_attach (Left 1)
  return ()

-- |Load file and its symbols
file :: (MonadIO m) => FilePath -> GdbT m ()
file fp = do
  cmd' R.RCDone $ C.file_exec_and_symbols (Just fp)

-- |Upload file to target device
load :: (MonadIO m) => GdbT m [R.Result]
load = do
  cmd R.RCDone $ C.target_download
