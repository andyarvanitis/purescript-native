[![PureScript](https://raw.githubusercontent.com/purescript/purescript/master/logo.png)](http://purescript.org)

**Pure11** is an experimental C++11/native compiler backend for [PureScript](https://github.com/purescript/purescript). It attempts to generate "sane", performant, and portable C++11 code as an intermediate language, which is then compiled to a native executable binary. This enables easy interoperability with existing C/C++ frameworks and libraries on a number of platforms.

---

### **Please note that this project and [purescript-native](https://github.com/pure11/purescript-native) are now the same**

---

#### Performance

* No runtime system (beyond some support classes and the standard C++11 runtime library)
* For automatic memory management, uses either native C++11 reference counting (`std::shared_ptr`), or the [Boehm-Demers-Weiser Garbage Collector](http://hboehm.info/gc/) -- **selectable when building** (see instructions below)
* Uses PureScript's normal tail call optimizations for generated C++11 code

#### Differences from PureScript:

* Foreign imports are C++11 (or C) instead of JavaScript ([FFI examples](https://github.com/pure11/pure11/wiki/FFI_Examples))
* Foreign functions are not written as curried functions (any necessary currying is done implicitly by the compiler)
* Compiler is `pcc` instead of `psc`, `make` is recommended for building
* No Pure11-specific REPL

#### Other notes:

* PureScript arrays are implmented using [`std::deque`](http://en.cppreference.com/w/cpp/container/deque) (random access *O(1)*)
* `String` types are implmented using either C-strings (for literals) or `std::string`
* `Number` is C++ `double`, `Int` is C++ `int`, `Char` is `char`, `Boolean` is `bool`
* [The current tested/supported package set is here.](https://github.com/pure11/package-sets/blob/psc-0.10.2/packages.json) Please feel free to submit a PR if you've had success with any others.

#### TO-DO:

* Get automated builds/tests fully up and running [(some work already done)](https://github.com/pure11/purescript/blob/pure11/pcc/TestMain.hs)
* Unicode (UTF-8) support for `String` (possibly use code from my Idris backend)
* Lots of testing!

#### Future ideas:

* Nice facilities (modules) for concurrency/parallelism, using `std::thread`, `std::async`, etc. under the hood
* `BigInt` via GNU GMP (or an alternative)

#### Requirements

* Everything you need to build [PureScript](https://github.com/purescript/purescript)
* A C++11-capable toolchain, e.g. recent versions of clang, gcc, Microsoft Visual Studio 2015
* GNU Make is the default supported build tool, but you should be able to use your favorite C++ build system, tools, debuggers, etc.

#### Examples

**`fib.purs`**
```PureScript
module Main where
  import Prelude
  import Control.Monad.Eff.Console (log, logShow)

  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n - 2) + fib (n - 1)

  main = do
    log "Here's the result of fib 10:"
    logShow (fib 10)
```
**`fib.cc`**
```c++
#include "Main/Main.hh"
namespace Main {
  using namespace PureScript;
  using namespace Prelude;
  using namespace Control_Monad_Eff_Console;
  using namespace Data_Semiring;
  using namespace Data_Ring;
  using namespace Control_Bind;
  using namespace Control_Monad_Eff;
  using namespace Data_Show;
  
  auto fib(int v) -> int {
    switch (cast<int>(v)) {
      case 0: return 0;
      case 1: return 1;
    };
    return fib(v - 2) + fib(v - 1);
  };
  auto main() -> any {
    Control_Monad_Eff_Console::log("Here's the result of fib 10:")();
    return Control_Monad_Eff_Console::logShow(Data_Show::showInt, fib(10))();
  };
}

auto main(int, char *[]) -> int {
  using namespace Main;
  INITIALIZE_GC();
  Main::main();
  return 0;
};
```
---
#### [FFI examples](https://github.com/pure11/pure11/wiki/FFI_Examples)
---
#### Getting Started
This assumes you are running OS X or a Unix-like system (Linux, *BSD, etc.), and already have the ability to [build PureScript from source] (http://www.purescript.org/download/) (bottom of the page) -- but instead of cloning and building the repository in the instructions, make sure to use [this project's repo](https://github.com/pure11/purescript).

1. Make sure you have developer tools for your system installed. For OS X, you'll need a recent version of Xcode. For Linux, etc., you need gcc 4.9.2 or later, including g++ support. You can also use clang 3.5 or later, but it still requires gcc for its C++ standard libraries.

2. **Optional and advanced:** If you wish to use the Boehm garbage collector (better runtime performance, in general, but less tested), install it for your system. For OS X, [brew](http://brew.sh/) is an easy method and recommended (package `bdw-gc`). For linux, it's available under package names such as `libgc-dev` (for Debian/Ubuntu).

3. Create a working directory wherever you like, and a `src` subdirectory under it, which will be where you will place your own PureScript source files.

4. From your working directory, run the `{installation_path}/pcc` command with no arguments. This will generate `Makefile` and `psc-package.json` files in that directory. There are some usage notes in the generated `Makefile`.

5. Use PureScript's standard `psc-package` utility to add and manage package dependencies.

6. You should now be ready to build a PureScript program.
  * As stated above, place your source file(s) in the working directory's `src` subdirectory and execute `make`. If your machine has multiple cores, you might want to use `make -jN`, where `N` is the number of cores.
  
  * **Optional and advanced:** To use the garbage collector, add `GC=yes` to the `make` command line (or you can modify the `Makefile` that you generated in step 4).

  * This will generate the C++ source tree for your program and then build an executable binary. The resulting executable will be in the `bin` subdirectory under the output directory and called `main` (so `output/bin/main`, by default).

---
