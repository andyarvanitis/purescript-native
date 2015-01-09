module Prelude
  ( otherwise
  , Int(..)
  , Integer(..)
  , flip
  , const
  , asTypeOf
  , Semigroupoid, (<<<), (>>>)
  , Category, id
  , ($), (#)
  , (:), cons
  , Show, show
  , Functor, (<$>), (<#>), void
  , Apply, (<*>)
  , Applicative, pure, liftA1
  , Bind, (>>=)
  , Monad, return, liftM1, ap
  , Num, (+), (-), (*), (/), (%)
  , negate
  , Eq, (==), (/=), refEq, refIneq
  , Ord, Ordering(..), compare, (<), (>), (<=), (>=)
  , Bits, (.&.), (.|.), (.^.), shl, shr, zshr, complement
  , BoolLike, (&&), (||)
  , not
  , Semigroup, (<>), (++)
  , Unit(..), unit
  ) where

  otherwise :: Boolean
  otherwise = true

  type Int = Number
  type Integer = Number -- TODO: use proper bignum
  -- TODO: define float type(s)

  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  const :: forall a b. a -> b -> a
  const a _ = a

  asTypeOf :: forall a. a -> a -> a
  asTypeOf x _ = x

  infixr 9 >>>
  infixr 9 <<<

  class Semigroupoid a where
    (<<<) :: forall b c d. a c d -> a b c -> a b d

  instance semigroupoidArr :: Semigroupoid (->) where
    (<<<) f g x = f (g x)

  (>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
  (>>>) f g = g <<< f

  class (Semigroupoid a) <= Category a where
    id :: forall t. a t t

  instance categoryArr :: Category (->) where
    id x = x

  infixr 0 $
  infixl 0 #

  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  infixr 6 :

  (:) :: forall a. a -> [a] -> [a]
  (:) = cons

  foreign import _cons
    "func _cons(e interface{}) interface{} {\
    \  return func(l interface{}) interface{} {\
    \    return append(l.([]interface{}), e);\
    \  };\
    \}" :: forall a. a -> [a] -> [a]

  cons = _cons

  class Show a where
    show :: a -> String

  foreign import _showStringImpl """
    func _showStringImpl(s interface{}) interface{} {
      return fmt.Sprintf("\"%s\"", s);
    }""" :: String -> String

  instance showUnit :: Show Unit where
    show (Unit {}) = "Unit {}"

  instance showString :: Show String where
    show = _showStringImpl

  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"

  foreign import _showNumberImpl "func _showNumberImpl(n interface{}) interface{} {\
                                \  return fmt.Sprint(n);\
                                \}" :: Number -> String

  instance showNumber :: Show Number where
    show = _showNumberImpl

  foreign import _showArrayImpl """
  func _showArrayImpl(f_ interface{}) interface{} {
    f := f_.(func (interface{}) interface{})
    return func(xs_ interface{}) interface{} {
      xs := xs_.([]interface{})
      last := len(xs) - 1
      var ss string
      for i, x := range xs {
        ss += f(x).(string)
        if last > 0 && i < last {
          ss += ","
        }
      }
      return "[" + ss + "]"
    }
  }""" :: forall a. (a -> String) -> [a] -> String

  instance showArray :: (Show a) => Show [a] where
    show = _showArrayImpl show

  infixl 4 <$>
  infixl 1 <#>

  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  (<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
  (<#>) fa f = f <$> fa

  void :: forall f a. (Functor f) => f a -> f Unit
  void fa = const unit <$> fa

  infixl 4 <*>

  class (Functor f) <= Apply f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  class (Apply f) <= Applicative f where
    pure :: forall a. a -> f a

  liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
  liftA1 f a = pure f <*> a

  infixl 1 >>=

  class (Apply m) <= Bind m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  class (Applicative m, Bind m) <= Monad m

  return :: forall m a. (Monad m) => a -> m a
  return = pure

  liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
  liftM1 f a = do
    a' <- a
    return (f a')

  ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
  ap f a = do
    f' <- f
    a' <- a
    return (f' a')

  instance functorArr :: Functor ((->) r) where
    (<$>) = (<<<)

  instance applyArr :: Apply ((->) r) where
    (<*>) f g x = f x (g x)

  instance applicativeArr :: Applicative ((->) r) where
    pure = const

  instance bindArr :: Bind ((->) r) where
    (>>=) m f x = f (m x) x

  instance monadArr :: Monad ((->) r)

  infixl 7 *
  infixl 7 /
  infixl 7 %

  infixl 6 -
  infixl 6 +

  class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
    (%) :: a -> a -> a
    negate :: a -> a

  foreign import _numAdd "func _numAdd(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(int) + n2.(int);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numSub "func _numSub(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(int) - n2.(int);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numMul "func _numMul(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(int) * n2.(int);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numDiv "func _numDiv(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(int) / n2.(int);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numMod "func _numMod(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(int) % n2.(int);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numNegate "func _numNegate(n interface{}) interface{} {\
                           \  return -(n.(int));\
                           \}" :: Number -> Number

  instance numNumber :: Num Number where
    (+) = _numAdd
    (-) = _numSub
    (*) = _numMul
    (/) = _numDiv
    (%) = _numMod
    negate = _numNegate

  newtype Unit = Unit {}

  unit :: Unit
  unit = Unit {}

  infix 4 ==
  infix 4 /=

  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

  foreign import _refEq
    "func _refEq(r1 interface{}) interface{} {\
    \  return func(r2 interface{}) interface{} {\
    \    return r1 == r2;\
    \  };\
    \}" :: forall a. a -> a -> Boolean

  refEq = _refEq

  foreign import _refIneq
    "func _refIneq(r1 interface{}) interface{} {\
    \  return func(r2 interface{}) interface{} {\
    \    return r1 != r2;\
    \  };\
    \}" :: forall a. a -> a -> Boolean

  refIneq = _refIneq

  instance eqUnit :: Eq Unit where
    (==) (Unit {}) (Unit {}) = true
    (/=) (Unit {}) (Unit {}) = false

  instance eqString :: Eq String where
    (==) = refEq
    (/=) = refIneq

  instance eqNumber :: Eq Number where
    (==) = refEq
    (/=) = refIneq

  instance eqBoolean :: Eq Boolean where
    (==) = refEq
    (/=) = refIneq

  foreign import _eqArrayImpl"""
    func _eqArrayImpl(f interface{}) interface{} {
      panic("eqArrayImpl not implemented")
    }""" :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> Boolean

  instance eqArray :: (Eq a) => Eq [a] where
    (==) xs ys = _eqArrayImpl (==) xs ys
    (/=) xs ys = not (xs == ys)

  data Ordering = LT | GT | EQ

  instance eqOrdering :: Eq Ordering where
    (==) LT LT = true
    (==) GT GT = true
    (==) EQ EQ = true
    (==) _  _  = false
    (/=) x y = not (x == y)

  instance showOrdering :: Show Ordering where
    show LT = "LT"
    show GT = "GT"
    show EQ = "EQ"

  class (Eq a) <= Ord a where
    compare :: a -> a -> Ordering

  infixl 4 <

  (<) :: forall a. (Ord a) => a -> a -> Boolean
  (<) a1 a2 = case a1 `compare` a2 of
    LT -> true
    _ -> false

  infixl 4 >

  (>) :: forall a. (Ord a) => a -> a -> Boolean
  (>) a1 a2 = case a1 `compare` a2 of
    GT -> true
    _ -> false

  infixl 4 <=

  (<=) :: forall a. (Ord a) => a -> a -> Boolean
  (<=) a1 a2 = case a1 `compare` a2 of
    GT -> false
    _ -> true

  infixl 4 >=

  (>=) :: forall a. (Ord a) => a -> a -> Boolean
  (>=) a1 a2 = case a1 `compare` a2 of
    LT -> false
    _ -> true

  foreign import _unsafeCompareImpl """
    func _unsafeCompareImpl(lt interface{})interface{} {
      return func(eq interface{}) interface{} {
        return func(gt interface{}) interface{} {
          return func(x interface{}) interface{} {
            return func(y interface{}) interface{} {
              if x.(int) < y.(int) {
                return lt
              } else if x.(int) > y.(int) {
                return gt
              } else {
                return eq
              }
            }
          }
        }
      }
    }""" :: forall a. Ordering -> Ordering -> Ordering -> a -> a -> Ordering

  unsafeCompare :: forall a. a -> a -> Ordering
  unsafeCompare = _unsafeCompareImpl LT EQ GT

  instance ordUnit :: Ord Unit where
    compare (Unit {}) (Unit {}) = EQ

  instance ordBoolean :: Ord Boolean where
    compare false false = EQ
    compare false true  = LT
    compare true  true  = EQ
    compare true  false = GT

  instance ordNumber :: Ord Number where
    compare = unsafeCompare

  instance ordString :: Ord String where
    compare = unsafeCompare

  instance ordArray :: (Ord a) => Ord [a] where
    compare [] [] = EQ
    compare [] _ = LT
    compare _ [] = GT
    compare (x:xs) (y:ys) = case compare x y of
      EQ -> compare xs ys
      other -> other

  infixl 10 .&.
  infixl 10 .|.
  infixl 10 .^.

  class Bits b where
    (.&.) :: b -> b -> b
    (.|.) :: b -> b -> b
    (.^.) :: b -> b -> b
    shl :: b -> Number -> b
    shr :: b -> Number -> b
    zshr :: b -> Number -> b
    complement :: b -> b

  foreign import _numShl "func _numShl(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(uint) << n2.(uint);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numShr "func _numShr(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(uint) >> n2.(uint);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numZshr "func _numZshr(n1 interface{}) interface{} {\
                          \  return func(n2 interface{}) interface{} {\
                          \    panic(\"n1 >>> n2\");\
                          \  };\
                          \}" :: Number -> Number -> Number

  foreign import _numAnd "func _numAnd(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(uint) & n2.(uint);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numOr "func _numOr(n1 interface{}) interface{} {\
                       \  return func(n2 interface{}) interface{} {\
                       \    return n1.(uint) | n2.(uint);\
                       \  };\
                       \}" :: Number -> Number -> Number

  foreign import _numXor "func _numXor(n1 interface{}) interface{} {\
                        \  return func(n2 interface{}) interface{} {\
                        \    return n1.(uint) ^ n2.(uint);\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import _numComplement "func _numComplement(n interface{}) interface{} {\
                               \  return ^(n.(uint));\
                               \}" :: Number -> Number

  instance bitsNumber :: Bits Number where
    (.&.) = _numAnd
    (.|.) = _numOr
    (.^.) = _numXor
    shl = _numShl
    shr = _numShr
    zshr = _numZshr
    complement = _numComplement

  infixr 2 ||
  infixr 3 &&

  class BoolLike b where
    (&&) :: b -> b -> b
    (||) :: b -> b -> b
    not :: b -> b

  foreign import _boolAnd "func _boolAnd(b1 interface{}) interface{} {\
                         \  return func(b2 interface{}) interface{} {\
                         \    return b1.(bool) && b2.(bool);\
                         \  };\
                         \}"  :: Boolean -> Boolean -> Boolean

  foreign import _boolOr "func _boolOr(b1 interface{}) interface{} {\
                        \  return func(b2 interface{}) interface{} {\
                        \    return b1.(bool) || b2.(bool);\
                        \  };\
                        \}" :: Boolean -> Boolean -> Boolean

  foreign import _boolNot "func _boolNot(b interface{}) interface{} {\
                         \  return !(b.(bool));\
                         \}" :: Boolean -> Boolean

  instance boolLikeBoolean :: BoolLike Boolean where
    (&&) = _boolAnd
    (||) = _boolOr
    not = _boolNot

  infixr 5 <>

  class Semigroup a where
    (<>) :: a -> a -> a

  foreign import _concatString
    "func _concatString(s1 interface{}) interface{} {\
    \  return func(s2 interface{}) interface{} {\
    \    return s1.(string) + s2.(string);\
    \  };\
    \}" :: String -> String -> String

  instance semigroupUnit :: Semigroup Unit where
    (<>) (Unit {}) (Unit {}) = Unit {}

  instance semigroupString :: Semigroup String where
    (<>) = _concatString

  instance semigroupArr :: (Semigroup s') => Semigroup (s -> s') where
    (<>) f g = \x -> f x <> g x

  infixr 5 ++

  (++) :: forall s. (Semigroup s) => s -> s -> s
  (++) = (<>)

module Data.Function where

  on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
  on f g x y = g x `f` g y

  foreign import data Fn0 :: * -> *
  foreign import data Fn1 :: * -> * -> *
  foreign import data Fn2 :: * -> * -> * -> *
  foreign import data Fn3 :: * -> * -> * -> * -> *
  foreign import data Fn4 :: * -> * -> * -> * -> * -> *
  foreign import data Fn5 :: * -> * -> * -> * -> * -> * -> *
  foreign import data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *
  foreign import data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *
  foreign import data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *
  foreign import data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
  foreign import data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *

  foreign import _mkFn0
    "func _mkFn0(fn interface{}) interface{} {\
    \  return func(interface{}) interface{} {\
    \    return ApplyFn(fn);\
    \  };\
    \}" :: forall a. (Unit -> a) -> Fn0 a

  foreign import _mkFn1
    "func _mkFn1(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return ApplyFn(fn, a);\
    \  };\
    \}" :: forall a b. (a -> b) -> Fn1 a b

  foreign import _mkFn2
    "func _mkFn2(fn interface{}) interface{} {\
    \  return func(a, b interface{}) interface{} {\
    \    return ApplyFn(fn, a, b);\
    \  };\
    \}" :: forall a b c. (a -> b -> c) -> Fn2 a b c

  foreign import _mkFn3
    "func _mkFn3(fn interface{}) interface{} {\
    \  return func(a, b, c interface{}) interface{} {\
    \    return ApplyFn(fn, a, b, c);\
    \  };\
    \}" :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d

  foreign import _mkFn4
    "func _mkFn4(fn interface{}) interface{} {\
    \  return func(a, b, c, d interface{}) interface{} {\
    \    return ApplyFn(fn, a, b, c, d);\
    \  };\
    \}" :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e

  foreign import _mkFn5
    "func _mkFn5(fn interface{}) interface{} {\
    \  return func(a, b, c, d, e interface{}) interface{} {\
    \    return ApplyFn(fn, a, b, c, d, e);\
    \  };\
    \}" :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f

  foreign import _mkFn6
    "func _mkFn6(fn interface{}) interface{} {\
    \  return func(a, b, c, d, e, f interface{}) interface{} {\
    \    return ApplyFn(fn, a, b, c, d, e, f);\
    \  };\
    \}" :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g

  foreign import _mkFn7
    "func _mkFn7(fn interface{}) interface{} {\
    \  return func(a, b, c, d, e, f, g interface{}) interface{} {\
    \    return ApplyFn(fn, a, b, c, d, e, f, g);\
    \  };\
    \}" :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h

  foreign import _mkFn8
    "func _mkFn8(fn interface{}) interface{} {\
    \  return func(a, b, c, d, e, f, g, h interface{}) interface{} {\
    \    return ApplyFn(fn, a, b, c, d, e, f, g, h);\
    \  };\
    \}" :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i

  foreign import _mkFn9
    "func _mkFn9(fn interface{}) interface{} {\
    \  return func(a, b, c, d, e, f, g, h, i interface{}) interface{} {\
    \    return ApplyFn(fn, a, b, c, d, e, f, g, h, i);\
    \  };\
    \}" :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j

  foreign import _mkFn10
    "func _mkFn10(fn interface{}) interface{} {\
    \  return func(a, b, c, d, e, f, g, h, i, j interface{}) interface{} {\
    \    return ApplyFn(fn, a, b, c, d, e, f, g, h, i, j);\
    \  };\
    \}" :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k

  foreign import _runFn0
    "func _runFn0(fn interface{}) interface{} {\
    \  return ApplyFn(fn);\
    \}" :: forall a. Fn0 a -> a

  foreign import _runFn1
    "func _runFn1(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return ApplyFn(fn, a);\
    \  };\
    \}" :: forall a b. Fn1 a b -> a -> b

  foreign import _runFn2
    "func _runFn2(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return ApplyFn(fn, a, b);\
    \    };\
    \  };\
    \}" :: forall a b c. Fn2 a b c -> a -> b -> c

  foreign import _runFn3
    "func _runFn3(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return func(c interface{}) interface{} {\
    \        return ApplyFn(fn, a, b, c);\
    \      };\
    \    };\
    \  };\
    \}" :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d

  foreign import _runFn4
    "func _runFn4(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return func(c interface{}) interface{} {\
    \        return func(d interface{}) interface{} {\
    \          return ApplyFn(fn, a, b, c, d);\
    \        };\
    \      };\
    \    };\
    \  };\
    \}" :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e

  foreign import _runFn5
    "func _runFn5(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return func(c interface{}) interface{} {\
    \        return func(d interface{}) interface{} {\
    \          return func(e interface{}) interface{} {\
    \            return ApplyFn(fn, a, b, c, d, e);\
    \          };\
    \        };\
    \      };\
    \    };\
    \  };\
    \}" :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f

  foreign import _runFn6
    "func _runFn6(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return func(c interface{}) interface{} {\
    \        return func(d interface{}) interface{} {\
    \          return func(e interface{}) interface{} {\
    \            return func(f interface{}) interface{} {\
    \              return ApplyFn(fn, a, b, c, d, e, f);\
    \            };\
    \          };\
    \        };\
    \      };\
    \    };\
    \  };\
    \}" :: forall a b c d e f g. Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g

  foreign import _runFn7
    "func _runFn7(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return func(c interface{}) interface{} {\
    \        return func(d interface{}) interface{} {\
    \          return func(e interface{}) interface{} {\
    \            return func(f interface{}) interface{} {\
    \              return func(g interface{}) interface{} {\
    \                return ApplyFn(fn, a, b, c, d, e, f, g);\
    \              };\
    \            };\
    \          };\
    \        };\
    \      };\
    \    };\
    \  };\
    \}" :: forall a b c d e f g h. Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h

  foreign import _runFn8
    "func _runFn8(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return func(c interface{}) interface{} {\
    \        return func(d interface{}) interface{} {\
    \          return func(e interface{}) interface{} {\
    \            return func(f interface{}) interface{} {\
    \              return func(g interface{}) interface{} {\
    \                return func(h interface{}) interface{} {\
    \                  return ApplyFn(fn, a, b, c, d, e, f, g, h);\
    \                };\
    \              };\
    \            };\
    \          };\
    \        };\
    \      };\
    \    };\
    \  };\
    \}" :: forall a b c d e f g h i. Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i

  foreign import _runFn9
    "func _runFn9(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return func(c interface{}) interface{} {\
    \        return func(d interface{}) interface{} {\
    \          return func(e interface{}) interface{} {\
    \            return func(f interface{}) interface{} {\
    \              return func(g interface{}) interface{} {\
    \                return func(h interface{}) interface{} {\
    \                  return func(i interface{}) interface{} {\
    \                    return ApplyFn(fn, a, b, c, d, e, f, g, h, i);\
    \                  };\
    \                };\
    \              };\
    \            };\
    \          };\
    \        };\
    \      };\
    \    };\
    \  };\
    \}" :: forall a b c d e f g h i j. Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j

  foreign import _runFn10
    "func _runFn10(fn interface{}) interface{} {\
    \  return func(a interface{}) interface{} {\
    \    return func(b interface{}) interface{} {\
    \      return func(c interface{}) interface{} {\
    \        return func(d interface{}) interface{} {\
    \          return func(e interface{}) interface{} {\
    \            return func(f interface{}) interface{} {\
    \              return func(g interface{}) interface{} {\
    \                return func(h interface{}) interface{} {\
    \                  return func(i interface{}) interface{} {\
    \                    return func(j interface{}) interface{} {\
    \                      return ApplyFn(fn, a, b, c, d, e, f, g, h, i, j);\
    \                    };\
    \                  };\
    \                };\
    \              };\
    \            };\
    \          };\
    \        };\
    \      };\
    \    };\
    \  };\
    \}" :: forall a b c d e f g h i j k. Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k

module Data.Eq where

  newtype Ref a = Ref a

  liftRef :: forall a b. (a -> a -> b) -> Ref a -> Ref a -> b
  liftRef f (Ref x) (Ref y) = f x y

  instance eqRef :: Eq (Ref a) where
    (==) = liftRef refEq
    (/=) = liftRef refIneq

  instance functorRef :: Functor Ref where
    (<$>) f (Ref x) = Ref (f x)

module Prelude.Unsafe where

  foreign import _unsafeIndex """
    func _unsafeIndex(xs interface{}) interface{} {
      panic("unsafeIndex not implemented")
    }""" :: forall a. [a] -> Number -> a

module Control.Monad.Eff where

  foreign import data Eff :: # ! -> * -> *

  foreign import _returnE "func _returnE(a interface{}) interface{} {\
                         \    return a;\
                         \}" :: forall e a. a -> Eff e a
  returnE = _returnE

  foreign import _bindE """
    func _bindE(a_ interface{}) interface{} {
      a := a_.(func (interface{}) interface{})
      return func(f_ interface{}) interface{} {
        f := f_.(func (interface{}) interface{})
        return func(interface{}) interface{} {
          return f(a(nil)).(func (interface{}) interface{})(nil)
        }
      }
    }""" :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

  bindE = _bindE

  type Pure a = forall e. Eff e a

  foreign import _runPure """
  func _runPure(f_ interface{}) interface{} {
    f := f_.(func (interface{}) interface{})
    return f(nil)
  }""" :: forall a. Pure a -> a

  runPure = _runPure

  instance functorEff :: Functor (Eff e) where
    (<$>) = liftA1

  instance applyEff :: Apply (Eff e) where
    (<*>) = ap

  instance applicativeEff :: Applicative (Eff e) where
    pure = returnE

  instance bindEff :: Bind (Eff e) where
    (>>=) = bindE

  instance monadEff :: Monad (Eff e)

  foreign import _untilE """
    func _untilE(f interface{}) interface{} {
      panic("untilE not implemented")
    }""" :: forall e. Eff e Boolean -> Eff e Unit

  foreign import _whileE """
    func _whileE(f interface{}) interface{} {
      panic("whileE not implemented")
    }""" :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit

  foreign import _forE """
    func _forE(lo interface{}) interface{} {
      panic("forE not implemented")
    }""" :: forall e. Number -> Number -> (Number -> Eff e Unit) -> Eff e Unit

  foreign import _foreachE """
    func _foreachE(as interface{}) interface{} {
      panic("foreachE not implemented")
    }""" :: forall e a. [a] -> (a -> Eff e Unit) -> Eff e Unit

module Control.Monad.Eff.Unsafe where

  import Control.Monad.Eff

  foreign import _unsafeInterleaveEff
    "func _unsafeInterleaveEff(f interface{}) interface{} {\
    \  return f;\
    \}" :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a

  _ignoreUnused = runPure

module Debug.Trace where

  import Control.Monad.Eff

  foreign import data Trace :: !

  foreign import _trace """
    func _trace(s interface{}) interface{} {
      return func(interface{}) interface{} {
        fmt.Println(s)
        return nil
      }
    }""" :: forall r. String -> Eff (trace :: Trace | r) Unit

  trace = _trace

  print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) Unit
  print o = trace (show o)

  _ignoreUnused = runPure

module Control.Monad.ST where

  import Control.Monad.Eff

  foreign import data ST :: * -> !

  foreign import data STRef :: * -> * -> *

  foreign import _newSTRef """
  func _newSTRef(val interface{}) interface{} {
    return func(interface{}) interface{} {
      return &val
    }
  }""" :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

  foreign import _readSTRef """
  func _readSTRef(ref interface{}) interface{} {
    return func(interface{}) interface{} {
      var ref = ref.(*interface{})
      return *ref
    }
  }""" :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

  foreign import _modifySTRef """
  func _modifySTRef(ref interface{}) interface{} {
    return func(f interface{}) interface{} {
      return func(interface{}) interface{} {
        var ref = ref.(*interface{})
        *ref = f.(func (interface{}) interface{})(*ref)
        return *ref
      }
    }
  }""" :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a

  foreign import _writeSTRef """
  func _writeSTRef(ref interface{}) interface{} {
    return func(a interface{}) interface{} {
      return func(interface{}) interface{} {
        var ref = ref.(*interface{})
        *ref = a
        return *ref
      }
    }
  }""" :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) a

  foreign import _runST """
  func _runST(f interface{}) interface{} {
    return f
  }""" :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a

  runST = _runST

  pureST :: forall a. (forall h r. Eff (st :: ST h | r) a) -> a
  pureST st = runPure (runST st)
