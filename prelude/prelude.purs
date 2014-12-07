module Prelude where

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a

instance numNumber :: Num Number where
  (+) = numAdd
  (-) = numSub

foreign import numAdd "func numAdd(n1 int) func (int) int {\
                      \  return func(n2 int) int {\
                      \    return n1 + n2\
                      \  }\
                      \}" :: Number -> Number -> Number

foreign import numSub "func numSub(n1 int) func (int) int {\
                      \  return func(n2 int) int {\
                      \    return n1 - n2\
                      \  }\
                      \}" :: Number -> Number -> Number
