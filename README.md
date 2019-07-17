[![PureScript](https://raw.githubusercontent.com/purescript/purescript/master/logo.png)](http://purescript.org)

This is an experimental [Go](https://golang.org) compiler backend for [PureScript](https://github.com/purescript/purescript). It attempts to generate "sane", debuggable, and portable Go code as an intermediate language, which is then compiled to a native executable binary. This enables easy interoperability with existing Go frameworks and libraries on a number of platforms.

#### Platforms
* Generated Go code and resulting binary executables tested (to at least some extent) on:
  * macOS Mojave 10.14.5 (full test suite)
  * Windows 10 x64 (full test suite)
  * Linux Debian 10 amd64 (full test suite)
  * ~~Raspberry Pi 3 B+ Raspbian official build (ARM)~~ (pending)

#### Performance

* No runtime system beyond some support types and functions
* Uses PureScript's normal tail call optimization techniques for generated Go code

#### Differences from PureScript:

* Foreign imports/exports are Go instead of JavaScript – see [standard library foreign implementations](https://github.com/andyarvanitis/purescript-native-go-ffi)
* No Go-specific REPL

#### Other notes:

* The provided `Any` type is just an alias for `interface{}`
* PureScript types are represented in Go with:

  | PureScript | Go |
  |------------|----|
  |`Array` | `[]Any`|
  |`String` | `string`|
  |`Number` | `float64`|
  |`Int` | `int`|
  |`Char` | `string` (single UTF-8 entity)|
  |`Boolean` | `bool`|
  |Records | `map[string]Any`|

#### Future ideas:

* Nice facilities (modules) for concurrency/parallelism, using goroutines

#### Requirements for building the compiler itself

* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (if you're running macOS 10.13+, you can use pre-built binaries from [here](https://github.com/andyarvanitis/purescript-native/releases/))

#### Requirements for using it

* A recent version (0.13+) of [purescript](https://github.com/purescript/purescript/releases).

* The [Go toolchain](https://golang.org) for your system.

* Your favorite PureScript package manager and build tools – but for simplicity these instructions will use [`spago`](https://github.com/spacchetti/spago).

#### Getting Started
This assumes you are running macOS (OS X) or a Unix-like system (Linux, *BSD, etc.).

1. Choose a working directory wherever you like. Then create a `src` subdirectory under it, which will be where you will place your own PureScript source files.

2. Under your working directory, clone or copy [purescript-native-go-ffi](https://github.com/andyarvanitis/purescript-native-go-ffi). Place any of your own foreign implementations into subdirectories of your working directory. Make sure your Go package directories are all under a `src` directory (standard Go practice).

3. Initialize your project with `spago init`, and install any dependencies with `spago install` (please see their instructions if you haven't used it before).

4. You should now be ready to build a PureScript program:
  * As stated above, place your source file(s) in the working directory's `src` subdirectory and execute
  
  ```sh
  spago build -- -g corefn && psgo
  ```

  * This will generate the Go source tree for your program and then build an executable binary. The resulting executable will be in your working directory and will be named `Main`. The generated source files will be under `output/src/`.

---
