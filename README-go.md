
### psgo

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


#### Getting Started
This assumes you are running macOS (OS X) or a Unix-like system (Linux, *BSD, etc.).

1. Choose a working directory wherever you like. Then create a `src` subdirectory under it, which will be where you will place your own PureScript source files.

1. Initialize your project with `spago init`, and install any dependencies with `spago install` (please see their instructions if you haven't used it before).

1. You should now be ready to build a PureScript program:
  * As stated above, place your source file(s) in the working directory's `src` subdirectory and execute
  
  ```sh
  spago build -- -g corefn && psgo
  ```

  * This will generate the Go source tree for your program and then build an executable binary. The resulting executable will be in your working directory and will be named `Main`. The generated source files will be under `output/`.

---