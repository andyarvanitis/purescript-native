///////////////////////////////////////////////////////////////////////////////
//
// Module      :  purescript.hh
// Copyright   :  (c) Andy Arvanitis 2016
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Includes and macros for garbage collection options
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PureScript_GC_HH
#define PureScript_GC_HH

#if defined(USE_GC)
  #define GC_THREADS
  #include <gc/gc_cpp.h>
  #include <gc/gc_allocator.h>
  #define WITH_ALLOCATOR(T) , traceable_allocator<T>
  #define SHARED_TYPE(T) T*
  #define MAKE_SHARED(T) new (GC) T
  #define IS_POINTER_TYPE(T) std::is_pointer<T>
  #define POINTER_FROM_MEMBER(P) P
  #define INITIALIZE_GC(_) GC_INIT()
#else
  #include <memory>
  #define WITH_ALLOCATOR(_)
  #define SHARED_TYPE(T) std::shared_ptr<T>
  #define MAKE_SHARED(T) std::make_shared<T>
  #define IS_POINTER_TYPE(T) std::is_assignable<shared<void>,T>
  #define POINTER_FROM_MEMBER(P) P.get()
  #define INITIALIZE_GC(_)
#endif

#endif // PureScript_GC_HH
