# hgdb

Gdb monad and REPL for reading and decoding registers built
around [hgdbmi](https://github.com/DistRap/hgdbmi)

## Usage

```haskell
import Gdb

main :: IO ()
main = do
  runGdb $ do
    file "image"
    breakpoint $ function_location "example"
    run
    stopped <- waitBreak
    echo $ show stopped
```

## `hgdb` application

Provided `hgdb` application is a proof of concept register viewer
utilizing repline for readline REPL allowing ad-hoc inspection
of embedded targets according to SVD (System View Description) files.


You can obtain SVD files from following repositories:
* [ada2svd](https://github.com/AdaCore/svd2ada/) (contains generic periherals for various Cortex-Ms)
* [cmsis-svd](https://github.com/posborne/cmsis-svd)

Or from [data-stm32](https://github.com/HaskellEmbedded/data-stm32)
repository root (ST only) with:

```bash
nix-build nix -A svdDb
```

### Inspecting registers

To use `arm-none-eabi-gdb` with [BlackMagicProbe](https://github.com/blacksphere/blackmagic)
available via `/dev/bmp` launch `hgdb` in following manner

```
hgdb --arm --bmp /dev/bmp --svd ~/svd2ada/CMSIS-SVD/Cortex_M/cm4f.svd
```

You can then inspect registers via REPL using their names
(tab completion available) delimited by comma, e.g.

```
λ> scb.scr
Register SCR
- System Control Register
- Address 0xE000ED10 (including offset 0x10)

0000000004
0x00000004
0b00000000000000000000000000000100
0b0000 0000 0000 0000 0000 0000 0000 0000 0100
Bit 2 SLEEPDEEP


+-------+---------+-+---------+-----------+-+
|◦[26:0]|SEVONPEND|◦|SLEEPDEEP|SLEEPONEXIT|◦|
+-------+---------+-+---------+-----------+-+
|   0   |    0    |0|    1    |     0     |0|
+-------+---------+-+---------+-----------+-+
```

### Command line options

* `-e | --ex` behaves like `gdb --ex`
* `--svd` specifies SVD file to load on start
* `--file` can be used to load image to Gdb on start
* `-a | --arm`  to use `arm-none-eabi-gdb`
* `--bmp DEV` for use with BlackMagicProbe over UART
* `--bmphosted HOST:PORT` for use with PC hosted BlackMagicProbe (`blackmagic_stlinkv2` binary)

For full list refer to `hgdb --help`

### Internal commands

* `:svd` - load SVD file, can be used instead of `--svd` arguments or to change current SVD file
* `:file` - load file to Gdb

All other REPL commands are forward to Gdb as CLI input.


## Build

Build with Nix via [ivory-tower-nix](https://github.com/HaskellEmbedded/ivory-tower-nix/)
by running

```bash
nix-build -A hgdb
```

## Notes

The interface is not final and will probably change. With more recent Gdb than
currently available on distributions we could also do completion for function names
and variables (requires `-symbol-list-functions` and `-symbol-list-variables`).

## Demo

[![asciicast](https://asciinema.org/a/300226.svg)](https://asciinema.org/a/300226)
