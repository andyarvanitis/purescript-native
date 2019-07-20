[![PureScript](https://raw.githubusercontent.com/purescript/purescript/master/logo.png)](http://purescript.org)

This is a platform-native compiler backend for [PureScript](https://github.com/purescript/purescript). It attempts to generate "sane", debuggable, and portable C++11 (or later) or [Go](https://golang.org/) code as an intermediate language, which is then compiled to a native executable binary. This also enables easy interoperability with existing C/C++ or Go frameworks and libraries on a number of platforms.

There are two utilities in the purescript-native "suite": `pscpp` and `psgo`. The source code in this branch is for `pscpp`, the purescript-to-C++ transpiler. For the source code for the `psgo` purescript-to-Go transpiler (and build tool), please see the [golang branch](https://github.com/andyarvanitis/purescript-native/tree/golang).

#### Platforms
* Although purescript-native should work on any platform supporting PureScript and modern C++ or Go, the generated code and resulting binary executables have been tested on:
  * macOS Mojave 10.14.5 – full test suite
      * For C++, Xcode 10.2.1's `clang` was used
      * For Go, `go` version 1.12.7 was used
  * Windows 10 x64 – full test suite
      * C++: Visual Studio 2017 and `clang`
      * Go: `go` version 1.12.7
  * Linux Debian 9.5 amd64 – full test suite (C++)
      * Default versions of `clang` and `gcc`
  * Linux Debian 10 amd64 – full test suite (Go)
      * Default version of `go`
  * Raspberry Pi 3 B+ Raspbian official build (ARM), default versions of `clang` and `gcc`


#### Requirements for building `pscpp` and `psgo`

* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (if you're running macOS 10.14.5+, you can use pre-built binaries from [here](https://github.com/andyarvanitis/purescript-native/releases/))

#### Requirements for using PureScript + purescript-native

* A recent version (0.13+) of [purescript](https://github.com/purescript/purescript/releases).

* For `pscpp`, a C++11-capable toolchain, e.g. recent versions of clang, gcc, Microsoft Visual Studio
    * GNU Make + `psc-package` is the default supported build tool, but you should be able to use your favorite package manager, C++ build system, tools, debuggers, etc.
* For `psgo`, the [Go toolchain](https://golang.org) for your system.
    * You can use your favorite PureScript package manager and build tools – but for simplicity, [`spago`](https://github.com/spacchetti/spago) is recommended.


#### For more information and a Getting Started guide, please see
* [`pscpp`](README-cpp.md)
* [`psgo`](https://github.com/andyarvanitis/purescript-native/blob/golang/README-go.md)


---
