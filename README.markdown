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

    fg-efb-server SOURCE_SPEC ...

This will start a web server; keep it running, and launch a suitable version of
the FlightGear E-Jet (one that has the EFB on board).

`SOURCE_SPEC` follows the format: *label*`$`*sourcetype*`$`*arg*`. Note that
`$` is a special shell character, so you need to escape it (`\\$`), or put the
whole spec in single quotes.

The *label* can be anything you like; this is what
the EFB will display inside FG.

*Sourcetype* must be one of:

- `file`: `arg` will be a local directory, and all the PDF files within it will
  be served, following the directory layout.
- `json`: `arg` should be a URL pattern, pointing to a JSON HTTP API following
  the Naviair format, e.g.:
    `https://aim.naviair.dk/umbraco/api/naviairapi/getnodesforparent?parentId={parentid}`
  The special token `{parentid}` is a placeholder for a numeric parent ID; it
  will be empty for the top-level listing, and hold the parent ID for all other
  listings. The API should return listings as JSON lists of objects containing,
  at the minimum, the following fields:
  - `id` (number; the ID of the entry itself)
  - `parentId` (number; the ID of the entry's parent; 0 if it's a top-level entry)
  - `title` (string)
  - `name` (string)
  - `href` (the URL of the associated PDF file, if any; may be `null`)
  - `isDir` (whether the entry is a directory)
  - `hasChildren` (whether the entry has any children)

You can define as many `SOURCE_SPEC`s as you like; they will be listed on the
front page of the FlightBag app in the order specified.

## Caching

Because downloading PDF files over HTTP costs a lot of bandwidth, and may make
you unpopular with chart providers, `fg-efb-server` will cache those files.

The default location of the cache is a subdirectory in the system temp
directory, using a random name starting with `fg-efb-cache`; this directory,
including all the cached files in it, will be deleted when the program
terminates.

You can override this by passing the environment variable `$PDFCACHE`; this
variable must point to an existing directory, and it will not be cleared out or
deleted when the process exits, allowing you to retain cached files between
runs.

## Bugs & Caveats

This is very alpha-quality code; use at your own risk.

Pretty much anything about it is subject to change without prior notice;
consider yourself warned.
