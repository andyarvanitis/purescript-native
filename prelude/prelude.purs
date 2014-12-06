module Prelude where

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a

instance numNumber :: Num Number where
  (+) = numAdd
  (-) = numSub

foreign import numAdd "function numAdd(n1) {\
                      \  return function(n2) {\
                      \    return n1 + n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numSub "function numSub(n1) {\
                      \  return function(n2) {\
                      \    return n1 - n2;\
                      \  };\
                      \}" :: Number -> Number -> Number
