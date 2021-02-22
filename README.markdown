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

    fg-efb-server

This will start a web server; keep it running, and launch a suitable version of
the FlightGear E-Jet (one that has the EFB on board).

## Data Sources Configuration

Data sources can be configured by editing the file `providers.yaml`. On
UNIX-like systems, this file will be searched in:

- `~/.config/fg-efb-server`
- `~/.fg-efb-server`

It should contain a dictionary of (arbitrary) keys to provider specs. See the
enclosed `providers.yaml.dist` example file. If no `providers.yaml` file was
found, a default configuration will be used, which exposes all directories and
PDF files in the current directory.

Some providers will interpolate variables into some of their configuration
values; these variables can be defined in `defs.yaml`, in the same directory as
the `providers.yaml` file.

### Provider Spec Keys (all providers)

- **`label: string`** - The label to be shown on the folder in-sim.
- **`type: string`** - The provider type. Must be one of `"file"` (scan a
  directory tree on a local disk), `"html"` (scrape from an HTML website on the
  internet), `"navaid"` (load from a JSON HTTP API in the format used by the
  Danish NavAID)

### `file` Spec Keys

- **`path: string`** - An absolute file path pointing to the root directory you
  want to expose.

### `html` Spec Keys

This one is probably the most complex one, as it needs to scrape HTML written
for consumption by humans rather than machines. This also means that getting
the configuration right for such a data source can be challenging.

- **`url: string`** - The URL root for the data source (protocol and
  hostname, e.g. `https://example.org`). This should not feature a trailing
  slash.
- **`start: string`** - The relative path to the start page, from the URL
  root (e.g. `/eAIS/start`). This is the first page that the flightbag will
  scrape when you open the data source.
  
  Any redirects will be followed, so for a typical eAIS website, it is a good
  idea to point to some page that links to the current version of the eAIS,
  rather than the eAIS itself (which usually has the release date encoded in
  its URL somehow).
- **`folders: [link-spec]`** - Specifies how to extract links to sub-folders
  from an HTML document
- **`documents: [link-spec]`** - Specifies how to extract links to PDF charts
  from an HTML document

Note that the `folders` and `documents` keys may contain several specs; they
will be executed in order.
A link spec may contain the following keys:

- **`select: string`** - A CSS selector pointing to DOM nodes to find.
  Example:
    - `div#chart-list a.chart`: "look for `<a>` elements with class `chart`, inside
      a `<div>` whose `id` is `chart-list`.
- **`href: null|string|object`** - What to extract as the link URL ("href")
  from any elements found. This can be one of the following:
    - `null` or the empty string `""`: get the text content of the element
    - A CSS selector string: get the text content of a child element
    - A string consisting of the `"@"` character, followed by an attribute
      name: get the value of that attribute from the element
    - An object containing two keys:
      - `child: string` - A CSS selector pointing to the child element to
        extract from; if absent, look at the element itself.
      - `attrib: string` - An attribute name (without `"@"`) to extract from;
        if absent, use the text content.
    If the entire `href` key is absent, it defaults to `"@href"` (get the
    `href=` attribute from the element itself).
- **`label: null|string|object`** - What to extract as the link label. The
  format is the same as for `href`; however, it defaults to the text content,
  rather than the `href=` attribute.
- **`format: Format`** - Transformations to apply to the link label, to make it
  prettier / more readable. `Format` can be any of the following:
    - `"basename"`: take the "basename", that is, remove everything up to the
      last path separator (slash), and strip the extension (e.g. `".pdf"`), if
      any. Ex.: `"http://example.com/eais/chart-123.pdf"` -> `"chart-123"`.
    - `"split-humps"`: parse as "camel case" or "Pascal case" (words without
      spaces, using uppercase to signal word boundaries), and put spaces
      between words. Ex.: `"ThisIsASentence"` -> `"This Is A Sentence"`.
    - `{split: "separator"}`: Split on each occurrence of the `"separator"`,
      and reassemble as words. Ex.:
      `"this-is-a-sentence"` --(`{split: "-"}`)-> `"this is a sentence"`.
    - `{replace: ["needle", "haystack"]}`: Replace each occurrence of the
      `"needle"` with `"haystack"`. Ex.:
      `"this-is-a-sentence"` --(`{replace: ["-", "."]}`)-> `"this.is.a.sentence"`.
    - `[Format, Format, ...]`: A list of `Format`s is processed in order,
      feeding the output from each formatting into the next formatting.

Note that the `href`, `label`, and `format` keys only apply to elements
selected through the `select` filter within the same link spec. This allows you
to apply different extraction and formatting patterns to different types of
links. For example, when extracting `<img>` tags, you will want to use
the `src` attribute to get the URL, and the `title` attribute for a label;
but when extracting `<a>` tags, you would want to use the `href` attribute for
URLs, and the text content for labels.

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
