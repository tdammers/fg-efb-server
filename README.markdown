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

### From Source

- `cabal v2-install`

### x86 Linux

The release tarball contains a self-contained ready-to-run executable; move it
anywhere on your `$PATH`, and it should Just Work (but see "Runtime
Dependencies" below).

## Runtime Dependencies

- `ImageMagick` - ideally, install version 7.0 or newer from
  [here](https://imagemagick.org/script/download.php); version 6.0 (which some
  Linux distros still ship) will likely work too, but may require some
  additional hoop-jumping (see below).
- `GhostScript` (ImageMagick uses this to convert PDF to PNG)

## Usage

Start the program with:

    fg-efb-server

This will start a web server; keep it running, and launch a suitable version of
the FlightGear E-Jet (one that has the EFB on board).

### Persisting Cached Files Across Runs

By default, `fg-efb-server` will create a temporary directory to cache
downloaded files, and deletes it when shutting down. If you want to retain
downloaded files, set the environment variable `PDFCACHE` to a directory of
your choice (which must exist when `fg-efb-server` starts up). Example:

    PDFCACHE=./cache fg-efb-server

Note that this will only cache PDF files and rendered JPG files; listings are
cached in-memory, and there is not currently a way of persisting them across
runs.

### Using legacy (6.x) ImageMagick

Before version 7.0, ImageMagick would ship as individual binaries by command,
and the conversion command we use is called `convert`, rather than `magick` as
with version 7.0 and up. If you want to use `convert`, you need to set the
environment variable `MAGICK_BINARY`, e.g.:

    MAGICK_BINARY=convert fg-efb-server

## Data Sources Configuration

Data sources can be configured by creating a file named `providers.yaml`.
On UNIX-like systems, this file will be searched in:

- `~/.config/fg-efb-server`
- `~/.fg-efb-server`

An example configuration file is provided as `providers.yaml.dist`, along with
a directory `provider-scripts`, which contains Lua scripts that some providers
reference.

It should contain a dictionary of (arbitrary) keys to provider specs. See the
enclosed `providers.yaml.dist` example file. If no `providers.yaml` file was
found, a default configuration will be used, which exposes all directories and
PDF files in the current directory.

Some providers will interpolate variables into some of their configuration
values; these variables can be defined in `defs.yaml`, in the same directory as
the `providers.yaml` file.

If you are unsure where to put these files, watch the console output from
`fg-efb-server`: if the files cannot be found, it will print a list of
locations searched.

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

### `lua` Spec Keys

The `lua` provider only needs a single key:

- **`script`** - A file path to a Lua script that defines the provider's
  behavior. File paths are interpreted relative to the directory where
  `providers.yaml` resides.

#### Lua Scripting

Lua provider scripts must meet the following criteria:

- The script must contain a function named `getPDF()`, which takes a single
  argument, `path`. This function should return a local path to a PDF file
  corresponding to the given `path`. What exactly `path` looks like is up to
  the script, but it should be a valid URL query path. The returned local path
  should normally come from a call to `http.download()` (see below), but you
  can of course also serve local PDF files directly.
- The script must contain a function named `listFiles()`, which takes a single
  argument, `path`, and returns a list of file descriptions. The empty string
  will be passed as the entry point for the provider.
- File descriptions returned by `listFiles()` are tables with the following
  keys:
  - `type`: must be either `"pdf"` (which will lead to `getPDF()`) or `"dir"`
    (which will lead to `listFiles()`).
  - `name`: a human-readable display name
  - `path`: the path that will be passed to `getPDF()` or `listFiles()` on
    subsequent calls.

The Lua host preloads all the default libraries, and provides some additional
built-in libraries:

- `http`: HTTP requests.
  - `http.download(url, extension)`: performs a cached HTTP GET request to the
    given `url`; the downloaded file is stored with the given `extension`, and
    the full path to that file is returned.
  - `http.get(url)`: performs a cached HTTP GET request to the given `url`, and
    returns the request body and the final `url` (after redirects).
- `url`: URL functionality.
  - `url.parse(str)`: parse the given string as a URL object. (You can simply
    convert a URL object back to a string to render it).
  - `url.join(left, right)`: parse the left and right strings as URL objects,
    join them, and return the result as a string. See below (URL objects,
    `__concat()`) for the rules by which URLs are joined. `url.join(a, b)` is
    shorthand for `tostring(url.parse(a) .. url.parse(b))`.
  - `encode(str)`: URL-encode the given string.
  - URL objects
    - `__concat()`: the concatenation operator for URLs will join them the same
      way a web browser would:
      - if the RHS is a full URL, it will simply replace the LHS
      - if it is a protocol-relative URL (starting with `//`), it will retain
        the LHS's protocol (`http:` or `https:`), but replace everything else
      - if it is a host-relative URL (starting with `/`), it will replace the
        entire path and query parts of the LHS, but retain the hostname and
        protocol
      - if it is a relative URL (starting with a query path, but no leading
        `/`), it will append the RHS's path and query to the LHS's path
        "directory" (retaining all but the last of the path parts)
      - if it is a query (starting with `?`), it will replace the LHS's
        query, but retain the protocol, host, and path
      - if it is an anchor (starting with `#`), it will replace the LHS's
        anchor part, but retain the rest
    - `.protocol`: `"http"` or `"https"`
    - `.host`: hostname (e.g. `"www.example.org"`)
    - `.path`: the URL path, as a list of parts (e.g. `foo/bar/baz` becomes
      `{"foo", "bar", "baz"}`).
    - `.query`: the query, as a key-value table
    - `.anchor`: string, everything after `#`.
- `xml`: XML DOM functionality.
  - `xml.mkName(local)`: make an XML Name object representing an unqualified
    local name (e.g. `<foobar/>`)
  - `xml.mkNameNS(local, namespaceURI)`: make an XML Name object representing a
    local name with a namespace URI (e.g. `<foobar xmlns="baz"/>`)
  - `xml.mkQName(prefix, local)`: make an XML Name object representing a name
    with a namespace prefix, in an unspecified namespace (e.g. `<foo:foobar
    />`).
  - `xml.mkQNameNS(prefix, local, namespace)`: make an XML Name object
    representing a name with a namespace prefix and a namespace URI (e.g.
    `<foo:foobar xmlns:foo="baz"/>`).
  - `xml.toname(str)`: parse a string as an XML Name object. Clarke notation is
    supported (e.g. `xml.toname("{baz}foobar")` amounts to `foobar
    xmlns="baz"`).
  - `xml.parseHTML(src)`: parses the given `src` string into an XML DOM object.
  - XML Name
    - `.local` - local name
    - `.namespace` - full namespace URI
    - `.prefix` - namespace alias prefix
  - XML Element
    - `.type` - always `"element"` for XML elements
    - `.name` - element name
    - `.attributes` - XML Attributes object
    - `.children` - list of child nodes
    - `.textContent` - text content, as per XML DOM spec
    - `:attr(name)` - shorthand for `.attributes[name]`
    - `:query(selector)` - run an extended CSS selector query, return list of
      result cursors.
  - XML Node
    - `.type` - one of `"element"`, `"comment"`, `"content"`, `"instruction"`
    - `.name` - node name (element name for elements, `nil` for others)
    - `.attributes` - XML Attributes object (elements only; `nil` for others)
    - `.children` - list of child nodes (empty list for non-element nodes)
    - `.textContent` - text content
    - `:attr(name)` - shorthand for `.attributes[name]` (returns `nil` for
      non-element nodes)
    - `:query(selector)` - run an extended CSS selector query, return list of
      result cursors.
  - XML Attribs (attribute list)
    - `.size`: number of attributes in the list.
    - `[]`: look up or set attribute by name. Accepts strings or XML Name
      objects for the keys.
  - XML Document
    - `.rootElement`: the document's root element
    - `:query(selector)`: run an extended CSS selector query, return list of
      result cursors.
  - XML Cursor (query result)
    - `.node`: the XML Node object that the cursor points to.
    - `:query(selector)`: run an extended CSS selector query, return list of
      result cursors.
    The reason we distinguish nodes from cursors is that cursors retain
    information about their siblings and ancestors, and to achieve this, they
    are implemented as an infinitely large lazy data structure behind the
    scenes, which makes it impossible to convert them to strict Lua data as-is.
    The `.node` field removes this extra information, making the data structure
    finite, but any further `:query()` calls on the node will not take the
    node's ancestory into consideration, but rather treat it as the root
    element of a self-contained DOM. This can occasionally be an advantage,
    though, because many queries will only need to consider a subset of a
    larger DOM tree, and by cutting that subtree off, we can make the query
    more efficient.

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

- The `lua` provider runs a full-blown Lua interpreter, and it is not
  sandboxed, meaning that any Lua scripts you have in your configuration can
  run arbitrary Lua code in the context of your server process. You should only
  run fg-efb-server against a configuration you trust.
- While we pass a memory limit to the `magick` process, this does not seem to
  work reliably, and it is possible for exceptionally large PDF files to make
  `magick` consume all your RAM and cause your system to come to a swapping
  grinding halt. If you are concerned about this, consider running
  fg-efb-server with strict `ulimit` constraints (at least on Linux, this
  should work).
- fg-efb-server does not detect the number of pages in a PDF, so it is not
  possible for the EFB to show a correct pager or prevent flipping past the
  last page.
