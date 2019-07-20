### pscpp


#### Performance

* No runtime system beyond some support classes and the standard C++11 (or later) runtime library
* For automatic memory management, uses native C++11 reference counting (`std::shared_ptr`)
* Uses PureScript's normal tail call optimization techniques for generated C++ code

#### Differences from PureScript:

* Foreign imports/exports are C++ instead of JavaScript – see [FFI notes](https://github.com/andyarvanitis/purescript-native/wiki/FFI) and [standard library foreign implementations](https://github.com/andyarvanitis/purescript-native-cpp-ffi)
* No C++-specific REPL

#### Other notes:

* PureScript arrays are implemented using [`std::vector`](http://en.cppreference.com/w/cpp/container/vector)
* `String` types are implemented with C++11 `u8` literals (UTF-8) and `std::string`
* `Number` is C++ `double`, `Int` is C++ `int`, `Char` is `std::string` (single UTF-8 entity), `Boolean` is `bool`

#### Future ideas:

* Nice facilities (modules) for concurrency/parallelism, using `std::thread`, `std::async`, etc. under the hood (output is already generally thread-safe for immutable values, thanks to `std::shared_ptr`)

#### Getting Started
This assumes you are running macOS (OS X) or a Unix-like system (Linux, *BSD, etc.).

1. Make sure you have developer tools for your system installed. For macOS, you'll need a recent version of Xcode. For Linux, etc., you can use clang 3.5 or later, or gcc/g++ 4.9.2 or later.

2. Create a working directory wherever you like, and a `src` subdirectory under it, which will be where you will place your own PureScript source files.

3. Under your working directory, also create an `ffi` subdirectory, which will be where you will place C/C++ FFI source files. You should at least add the contents of [purescript-native-cpp-ffi](https://github.com/andyarvanitis/purescript-native-cpp-ffi) into this directory, in addition to any of your own foreign implementations.

4. Generate the default GNU `Makefile` in your working directory by running `pscpp --makefile`.

5. Use PureScript's standard [`psc-package`](https://psc-package.readthedocs.io/en/latest/) utility to add and manage package dependencies.

6. You should now be ready to build a PureScript program:
  * As stated above, place your source file(s) in the working directory's `src` subdirectory and execute `make debug` or `make release`. If your build machine has multiple cores, you might want to append `-jN` to your *make* command, where `N` is the number of cores.

  * This will generate the C++ source tree for your program and then build an executable binary. The resulting executable will be in the `bin` subdirectory under the output directory and called `main` (so `output/bin/main`, by default). Source files will be under `src` (`output/src/` by default).

---
