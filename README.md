# microcosmos

*A tiny blogging platform*

See https://dixonary.co.uk/mc for more information.

## Build and Run

Prerequisites: 

* A version of the Haskell platform which includes `cabal`, if you are compiling it yourself.
* Libgmp

The program has only been tested on Linux but there is no obvious reason for it to break elsewhere.

#### Self-compiled

1. Grab a copy of microcosmos via clone or download the zip.
1. Run `cabal new-build`.
1. Either run directly from the `dist-newstyle` folder, or move that binary elsewhere, or use `cabal run`.

#### From `Releases`

1. Visit [releases](/dixonary/microcosmos/releases/).
1. Download the latest version for your system (currently Linux only).
1. Set file permissions: `$ chmod u+x ./mc` 
1. Run: `$ MC_PORT=[your port number] ./mc`

## How it works

You will probably want to read the source code (it's only 400 lines), but here's the gist:

* On first run, `mc` will populate the working directory with `posts`, `static`, and `template` folders along with some very basic contents.
* You can modify any of these files at runtime and the changes will be reflected on the website automatically.
* The templates include `<PLACEHOLDER name="...">` tags. These must be formatted exactly in this way, and the relevant variable is interposed into the template. The date is represented in [human](https://hackage.haskell.org/package/friendly-time) format.
* Adding a `.dirname` file to a folder will set the name of the directory as it appears in the relevant placeholders. 
