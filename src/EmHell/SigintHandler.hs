module EmHell.SigintHandler 
  ( sigintHandler
  ) where

import System.Posix.Signals (Handler(Catch))
import qualified Control.Concurrent
import qualified Control.Exception
import qualified System.Posix.Signals

sigintHandler :: IO b -> IO b
sigintHandler ofWhat = do
    tid <- Control.Concurrent.myThreadId
    Control.Exception.bracket
      (System.Posix.Signals.installHandler
         System.Posix.Signals.sigINT
         (Catch
            $ Control.Exception.throwTo
                tid
                Control.Exception.UserInterrupt
          )
         Nothing
      )
      (\old ->
         System.Posix.Signals.installHandler
           System.Posix.Signals.sigINT
           old
           Nothing
      )
      $ pure ofWhat
