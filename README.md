[![PureScript](https://raw.githubusercontent.com/purescript/purescript/master/logo.png)](http://purescript.org)

This is an experimental C++11 (or later) compiler backend for [PureScript](https://github.com/purescript/purescript). It attempts to generate "sane", debuggable, and portable C++ code as an intermediate language, which is then compiled to a native executable binary. This enables easy interoperability with existing C/C++ frameworks and libraries on a number of platforms.

---

### **Please note that this project supersedes [pure11](https://github.com/pure11/pure11) (they are no longer the same)**

#### Please see [changes](https://github.com/andyarvanitis/purescript-native/wiki/changes) for more details.

---

#### Platforms
* Generated C++ code and resulting binary executables tested (to at least some extent) on:
  * macOS High Sierra, Xcode 9.4.1's clang (full test suite)
  * Windows 10 x64 and x86, Visual Studio 2017
  * Linux Debian 9.5 amd64, default versions of clang and gcc/g++ (full test suite)
  * Raspberry Pi 3 B+ Raspbian official build (ARM), default versions of clang and gcc/g++

#### Performance

* No runtime system beyond some support classes and the standard C++11 (or later) runtime library
* For automatic memory management, uses native C++11 reference counting (`std::shared_ptr`)
* Uses PureScript's normal tail call optimization techniques for generated C++ code

#### Differences from PureScript:

* Foreign imports/exports are C++ instead of JavaScript – see [FFI notes](https://github.com/andyarvanitis/purescript-native/wiki/FFI) and [FFI standard library examples](https://github.com/andyarvanitis/purescript-native-ffi)
* No C++-specific REPL

#### Other notes:

* PureScript arrays are implemented using [`std::deque`](http://en.cppreference.com/w/cpp/container/deque) (random access *O(1)*)
* `String` types are implemented with C++11 `u8` literals (UTF-8) and `std::string`
* `Number` is C++ `double`, `Int` is C++ `int`, `Char` is `std::string` (single UTF-8 entity), `Boolean` is `bool`

#### Future ideas:

* Nice facilities (modules) for concurrency/parallelism, using `std::thread`, `std::async`, etc. under the hood (output is already generally thread-safe for immutable values, thanks to `std::shared_ptr`)

#### Requirements for building the compiler itself

* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (if you're running macOS 10.13+, you can use pre-built binaries from [here](https://github.com/andyarvanitis/purescript-native/releases/))

#### Requirements for using it

* A recent version (0.12+) of [purescript](https://github.com/purescript/purescript/releases).

* A C++11-capable toolchain, e.g. recent versions of clang, gcc, Microsoft Visual Studio
* GNU Make is the default supported build tool, but you should be able to use your favorite C++ build system, tools, debuggers, etc.

#### Getting Started
This assumes you are running macOS (OS X) or a Unix-like system (Linux, *BSD, etc.).

1. Make sure you have developer tools for your system installed. For macOS, you'll need a recent version of Xcode. For Linux, etc., you can use clang 3.5 or later, or gcc/g++ 4.9.2 or later.

2. Create a working directory wherever you like, and a `src` subdirectory under it, which will be where you will place your own PureScript source files.

3. Under your working directory, also create an `ffi` subdirectory, which will be where you will place C/C++ FFI source files. You should at least add the contents of [purescript-native-ffi](https://github.com/andyarvanitis/purescript-native-ffi) into this directory, in addition to any of your own foreign implementations.

4. Generate the default GNU `Makefile` in your working directory by running `pscpp --makefile`.

5. Use PureScript's standard [`psc-package`](https://psc-package.readthedocs.io/en/latest/) utility to add and manage package dependencies.

6. You should now be ready to build a PureScript program:
  * As stated above, place your source file(s) in the working directory's `src` subdirectory and execute `make`. If your machine has multiple cores, you might want to use `make -jN`, where `N` is the number of cores.

  * This will generate the C++ source tree for your program and then build an executable binary. The resulting executable will be in the `bin` subdirectory under the output directory and called `main` (so `output/bin/main`, by default). Source files will be under `src` (`output/src/` by default).

---
