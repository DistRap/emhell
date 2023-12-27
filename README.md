[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/DistRap/emhell/ci.yaml?branch=main)](https://github.com/DistRap/emhell/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/emhell.svg?color=success)](https://hackage.haskell.org/package/emhell)
[![Dependencies](https://img.shields.io/hackage-deps/v/emhell?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=emhell)

# emhell

Embedded development could be hell without a good tooling!

## Applications

### `emhell`

`emhell` is a SVD (System View Description) register browser

### `hocd`

`hocd` is a register viewer utilizing OpenOCD as a backend
using [`hocd`](https://github.com/DistRap/hocd)

### `hgdb`

`hgdb` is a register viewer and a `GDB` frontend,
built on top [`hgdbmi`](https://github.com/DistRap/hgdbmi)

#### Inspecting registers

To use `arm-none-eabi-gdb` with [BlackMagicProbe](https://github.com/blacksphere/blackmagic)
available via `/dev/bmp` launch `hgdb` in following manner

```
hgdb --arm --bmp /dev/bmp --svd stm32f407.svd
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

#### Command line options

* `-e | --ex` behaves like `gdb --ex`
* `--svd` specifies SVD file to load on start
* `--file` can be used to load image to Gdb on start
* `-a | --arm`  to use `arm-none-eabi-gdb`
* `--bmp DEV` for use with BlackMagicProbe over UART
* `--bmphosted HOST:PORT` for use with PC hosted BlackMagicProbe (`blackmagic_stlinkv2` binary)
* `--remotegdb HOST:PORT` for use with remotely running GDB server over TCP (could be OpenOCD provided one)

For full list refer to `hgdb --help`

#### Internal commands

* `:svd` - load SVD file, can be used instead of `--svd` arguments or to change current SVD file
* `:file` - load file to Gdb

All other REPL commands are forwarded to GDB as CLI input.

## Build

### Using Cabal

```bash
git clone https://github.com/DistRap/emhell
cabal build
```

### Using Nix

```bash
nix-build
```

## Notes

The interface is not final and will probably change. With more recent Gdb than
currently available on distributions we could also do completion for function names
and variables (requires `-symbol-list-functions` and `-symbol-list-variables`).

## Demo

[![asciicast](https://asciinema.org/a/300226.svg)](https://asciinema.org/a/300226)
