package ffi_loader

// Load PureScript standard library FFI packages. Comment out the ones you don't need for
// faster/smaller builds.

import (
	_ "github.com/purescript-native/go-ffi/purescript-arrays"
	_ "github.com/purescript-native/go-ffi/purescript-assert"
	_ "github.com/purescript-native/go-ffi/purescript-console"
	_ "github.com/purescript-native/go-ffi/purescript-effect"
	_ "github.com/purescript-native/go-ffi/purescript-foldable-traversable"
	_ "github.com/purescript-native/go-ffi/purescript-functions"
	_ "github.com/purescript-native/go-ffi/purescript-partial"
	_ "github.com/purescript-native/go-ffi/purescript-prelude"
	_ "github.com/purescript-native/go-ffi/purescript-random"
	_ "github.com/purescript-native/go-ffi/purescript-refs"
	_ "github.com/purescript-native/go-ffi/purescript-st"
	_ "github.com/purescript-native/go-ffi/purescript-strings"
)

// Add your own FFI packages here.

import ()
