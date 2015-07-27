///////////////////////////////////////////////////////////////////////////////
//
// Module      :  macros.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Support for various macros
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PureScript_Macros_HH
#define PureScript_Macros_HH

// Make a unique name using current line number and a counter

#define MAKE_UNIQUE_NAME( base ) JOIN_NAME_PARTS( base, __LINE__, __COUNTER__ )
#define JOIN_NAME_PARTS( p1, p2, p3 ) JOIN_NAME_PARTS_( p1, p2, p3 )
#define JOIN_NAME_PARTS_( p1, p2, p3 ) p1 ## p2 ## p3

// Used for "overloaded" variadic macros

#define VA_NUM_ARGS(...) VA_NUM_ARGS_(__VA_ARGS__, \
                                      30,29,28,27,26,25,24,23,22,21, \
                                      20,19,18,17,16,15,14,13,12,11, \
                                      10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

#define VA_NUM_ARGS_(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, \
                     _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
                     _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
                     N, ...) N

#define MACRO_VARIANT(func, ...) MACRO_VARIANT_(func, VA_NUM_ARGS(__VA_ARGS__))
#define MACRO_VARIANT_(func, nargs) MACRO_VARIANT__(func, nargs)
#define MACRO_VARIANT__(func, nargs) func ## _ ## nargs

#endif // PureScript_Macros_HH
