# FG-EFB-SERVER

## What is this?

`fg-efb-server` is an implementation of the FlightGear E-Jet EFB Companion
protocol.

It runs a local HTTP server on port 7675, exposing PDF files in a local
directory tree as PNG files.

For details, see:

https://github.com/tdammers/E-jet-family-YV/blob/wip/efb/Documentation/EFB.markdown  

## Build Requirements

- A working Haskell build toolchain, including GHC 8.10 or newer. Tested with
  Cabal 3.2; newer versions should be fine, too. Stack and Nix builds not
  currently supported, though it shouldn't be hard to make it work.

## Installation

- `cabal v2-install`

## Runtime Dependencies

- `ImageMagick`
- `GhostScript` (ImageMagick uses this to convert PDF to PNG)

## Usage

Start the program with:

    fg-efb-server $PDF_DIRECTORY

This will start a web server; keep it running, and launch a suitable version of
the FlightGear E-Jet (one that has the EFB on board). Opening the FlightBag app
should now show a listing of the PDF files and directories in `$PDF_DIRECTORY`.

## Bugs & Caveats

This is very alpha-quality code; use at your own risk.

Pretty much anything about it is subject to change without prior notice;
consider yourself warned.
