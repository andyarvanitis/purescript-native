///////////////////////////////////////////////////////////////////////////////
//
// Module      :  prelude_ffi.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Temporary prelude FFI stubs
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PreludeFFI_HH
#define PreludeFFI_HH

// These are just temporary Prelude FFI stubs. This file will go away after
// the PureScript FFI is refactored to handle non-JS FFIs.

#include "purescript.hh"

namespace Prelude {

  using namespace PureScript;

  namespace value {
    struct Unit_;
  }

  inline auto showStringImpl(string s) -> string {
    return s;
  }

  inline auto showNumberImpl(double x) -> string {
    return std::to_string(x);
  }

  inline auto showIntImpl(long x) -> string {
    return std::to_string(x);
  }

  inline auto showCharImpl(char x) -> string {
    return std::to_string(x);
  }

  template <typename A>
  inline auto showArrayImpl(fn<A,string> f) -> fn<list<A>,string> {
    return nullptr;
  }

  inline auto numAdd(double x) -> fn<double,double> {
    return [=](double y) {
      return x + y;
    };
  }

  inline auto numSub(double x) -> fn<double,double> {
    return [=](double y) {
      return x - y;
    };
  }

  inline auto numMul(double x) -> fn<double,double> {
    return [=](double y) {
      return x * y;
    };
  }

  inline auto numDiv(double x) -> fn<double,double> {
    return [=](double y) {
      return x / y;
    };
  }

  inline auto intAdd(long x) -> fn<long,long> {
    return [=](long y) {
      return x + y;
    };
  }

  inline auto intSub(long x) -> fn<long,long> {
    return [=](long y) {
      return x - y;
    };
  }

  inline auto intMul(long x) -> fn<long,long> {
    return [=](long y) {
      return x * y;
    };
  }

  inline auto intDiv(long x) -> fn<long,long> {
    return [=](long y) {
      return x / y;
    };
  }

  inline auto intMod(long x) -> fn<long,long> {
    return [=](long y) {
      return x % y;
    };
  }

  inline auto concatString(string x) -> fn<string,string> {
    return [=](string y) {
      return x + y;
    };
  }

  template <typename T>
  inline auto refEq(T ref1) -> fn<T,bool> {
    return [=](T ref2) {
      return ref1 == ref2;
    };
  }

  template <typename A>
  inline auto eqArrayImpl(fn<A,fn<A,bool>> f) -> fn<list<A>,fn<list<A>,bool>> {
    return [=](list<A> xs) {
      return [=](list<A> ys) {
        return false;
      };
    };
  }

  inline auto boolAnd(bool x) -> fn<bool,bool> {
    return [=](bool y) {
      return x && y;
    };
  }

  inline auto boolOr(bool x) -> fn<bool,bool> {
    return [=](bool y) {
      return x || y;
    };
  }

  inline auto boolNot(bool x) -> bool {
    return !x;
  }

  template <typename A>
  inline auto unsafeCompareImpl(A arg) -> fn<A,std::shared_ptr<void>> {
    return [=](A) {
      return nullptr;
    };
  }

  namespace type {
    struct Ordering;
  }
  template <typename T>
  inline auto unsafeCompareImpl(std::shared_ptr<type::Ordering> lt) ->
      fn<std::shared_ptr<type::Ordering>,fn<std::shared_ptr<type::Ordering>,fn<T,fn<T,std::shared_ptr<type::Ordering>>>>> {
    return nullptr;
  }

  template <typename A>
  inline auto concatArray(list<A> a) -> fn<list<A>,list<A>> {
    return [=](list<A> b) {
      return a.append(b);
    };
  }

  template <typename A, typename B>
  inline auto arrayMap(fn<A,B> f) -> fn<list<A>,list<B>> {
    return [=](list<A> a) {
      return list<B>();
    };
  }

  template <typename A, typename B>
  inline auto arrayBind(list<A> a) -> fn<fn<A,list<B>>,list<B>> {
    return [=](fn<A,list<B>> f) {
      return list<B>();
    };
  }

  template <typename A>
  inline auto ordArrayImpl(fn<A,fn<A,int>> f) -> fn<list<A>,fn<list<A>,int>> {
    return [=](list<A> xs) {
      return [=](list<A> ys) {
        return 0;
      };
    };
  }
}

namespace Control_Monad_Eff {

  using namespace PureScript;

  template <typename A, typename B>
  inline auto bindE(eff_fn<A> a) -> fn<fn<A,eff_fn<B>>,eff_fn<B>> {
    return [=](fn<A,eff_fn<B>> f) {
      return [=]() {
        return f(a())();
      };
    };
  }

  template <typename A>
  inline auto returnE(A a) -> eff_fn<A> {
    return [=]() {
      return a;
    };
  }
}

namespace Console {
  using namespace PureScript;
  inline auto log(string s) -> eff_fn<std::shared_ptr<Prelude::value::Unit_>> {
    return [=]() {
      puts(s.c_str());
      return nullptr;
    };
  }
}

namespace Test_Main {
  using namespace PureScript;
  inline auto throwError(string) -> eff_fn<std::shared_ptr<Prelude::value::Unit_>> {
    return nullptr;
  }
}


#endif // PreludeFFI_HH
