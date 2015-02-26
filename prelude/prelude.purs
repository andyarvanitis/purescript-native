module Prelude
  ( otherwise
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
  , Semiring, (+), zero, (*), one
  , ModuloSemiring, (/), mod
  , Ring, (-)
  , (%)
  , negate
  , DivisionRing
  , Num
  , Eq, (==), (/=) -- , refEq, refIneq
  , Ord, Ordering(..), compare, (<), (>), (<=), (>=)
  , Bits, (.&.), (.|.), (.^.), shl, shr, zshr, complement
  , BoolLike, (&&), (||)
  , not
  , Semigroup, (<>), (++)
  , Unit(..), unit
  ) where

  -- | An alias for `true`, which can be useful in guard clauses:
  -- |
  -- | ```purescript
  -- | max x y | x >= y = x
  -- |         | otherwise = y
  -- | ```
  -- |
  otherwise :: Boolean
  otherwise = true

  -- | Flips the order of the arguments to a function of two arguments.
  -- |
  -- | ```purescript
  -- | flip const 1 2 = const 2 1 = 2
  -- | ```
  -- |
  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  -- | Returns its first argument and ignores its second.
  -- |
  -- | ```purescript
  -- | const 1 "hello" = 1
  -- | ```
  -- |
  const :: forall a b. a -> b -> a
  const a _ = a

  -- | This function returns its first argument, and can be used to assert type equalities.
  -- | This can be useful when types are otherwise ambiguous.
  -- |
  -- | ```purescript
  -- | main = print $ [] `asTypeOf` [0]
  -- | ```
  -- |
  -- | If instead, we had written `main = print []`, the type of the argument `[]` would have
  -- | been ambiguous, resulting in a compile-time error.
  asTypeOf :: forall a. a -> a -> a
  asTypeOf x _ = x

  infixr 9 >>>
  infixr 9 <<<

  -- | A `Semigroupoid` is similar to a [`Category`](#category) but does not require an identity
  -- | element `id`, just composable morphisms.
  -- |
  -- | `Semigroupoid`s should obey the following rule:
  -- |
  -- | Association:
  -- |     `forall p q r. p <<< (q <<< r) = (p <<< q) <<< r`
  -- |
  class Semigroupoid a where
    (<<<) :: forall b c d. a c d -> a b c -> a b d

  instance semigroupoidArr :: Semigroupoid (->) where
    (<<<) f g x = f (g x)

  (>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
  (>>>) f g = g <<< f

  -- | `Category`s consist of objects and composable morphisms between them, and as such are
  -- | [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids` must have an identity element.
  -- |
  -- | `Category`s should obey the following rules.
  -- |
  -- | Left Identity:
  -- |     `forall p. id <<< p = p`
  -- |
  -- | Right Identity:
  -- |     `forall p. p <<< id = p`
  -- |
  class (Semigroupoid a) <= Category a where
    id :: forall t. a t t

  instance categoryArr :: Category (->) where
    id x = x

  infixr 0 $
  infixl 0 #

  -- | Applies a function to its argument
  -- |
  -- | ```purescript
  -- | length $ groupBy productCategory $ filter isInStock products
  -- | ```
  -- |
  -- | is equivalent to
  -- |
  -- | ```purescript
  -- | length (groupBy productCategory (filter isInStock (products)))
  -- | ```
  -- |
  -- | `($)` is different from [`(#)`](#-2) because it is right-infix instead of left, so
  -- | `a $ b $ c $ d x` = `a (b (c (d x)))`
  -- |
  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  -- | Applies a function to its argument
  -- |
  -- | ```purescript
  -- | products # groupBy productCategory # filter isInStock # length
  -- | ```
  -- |
  -- | is equivalent to
  -- |
  -- | ```purescript
  -- | length (groupBy productCategory (filter isInStock (products)))
  -- | ```
  -- |
  -- | `(#)` is different from [`($)`](#-1) because it is left-infix instead of right, so
  -- | `x # a # b # c # d` = `(((x a) b) c) d`
  -- |
  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  infixr 6 :

  -- | Attaches an element to the front of a list.
  -- |
  -- | ```purescript
  -- | 1 : [2, 3, 4] = [1, 2, 3, 4]
  -- | ```
  -- |
  (:) :: forall a. a -> [a] -> [a]
  (:) = cons

  foreign import cons
    """
    template <typename T>
    inline auto cons(T e) -> fn<list<T>, list<T>> {
      return [=](list<T> l) {
        return list<T>(e,l);
      };
    }
    """ :: forall a. a -> [a] -> [a]

  class Show a where
    show :: a -> String

  foreign import showStringImpl
    """
    inline auto showStringImpl(string s) -> string {
      return '"' + s + '"';
    }
    """ :: String -> String

  instance showUnit :: Show Unit where
    show (Unit {}) = "Unit {}"

  instance showString :: Show String where
    show = showStringImpl

  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"

  foreign import showNumberImpl
    """
    template <typename T>
    inline auto showNumberImpl(T n) -> string {
      return std::to_string(n);
    }
    """ :: Number -> String

  instance showNumber :: Show Number where
    show = showNumberImpl

  foreign import showArrayImpl
    """
    template <typename T>
    inline auto showArrayImpl(fn<T,string> f) -> fn<list<T>,string> {
      return [=](list<T> xs) -> string {
        string s("[");
        const auto sz = xs.size();
        for (auto i = 0; i < sz; i++) {
          s.append(f(xs[i]));
          if (i != sz - 1) s.append(",");
        }
        return s + "]";
      };
    }
    """ :: forall a. (a -> String) -> [a] -> String

  instance showArray :: (Show a) => Show [a] where
    show = showArrayImpl show

  infixl 4 <$>
  infixl 1 <#>

  -- | A `Functor` is intuitively a type which can be mapped over, and more formally a mapping
  -- | between [`Category`](#category)s that preserves structure.
  -- |
  -- | `Functor`s should obey the following rules.
  -- |
  -- | Identity:
  -- |     `(<$>) id = id`
  -- |
  -- | Composition:
  -- |     `forall f g. (<$>) (f . g) = ((<$>) f) . ((<$>) g)`
  -- |
  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  (<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
  (<#>) fa f = f <$> fa

  void :: forall f a. (Functor f) => f a -> f Unit
  void fa = const unit <$> fa

  infixl 4 <*>

  -- | `Apply`s are intuitively [`Applicative`](#applicative)s less `pure`, and more formally a
  -- | strong lax semi-monoidal endofunctor.
  -- |
  -- | `Apply`s should obey the following rules.
  -- |
  -- | Associative Composition:
  -- |     `forall f g h. (.) <$> f <*> g <*> h = f <*> (g <*> h)`
  -- |
  class (Functor f) <= Apply f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  -- | `Applicative`s are [`Functor`](#functor)s which can be "applied" by sequencing composition
  -- | (`<*>`) or embedding pure expressions (`pure`).
  -- |
  -- | `Applicative`s should obey the following rules.
  -- |
  -- | Identity:
  -- |     `forall v. (pure id) <*> v = v`
  -- |
  -- | Composition:
  -- |     `forall f g h. (pure (.)) <*> f <*> g <*> h = f <*> (g <*> h)`
  -- |
  -- | Homomorphism:
  -- |     `forall f x. (pure f) <*> (pure x) = pure (f x)`
  -- |
  -- | Interchange:
  -- |     `forall u y. u <*> (pure y) = (pure (($) y)) <*> u`
  -- |
  class (Apply f) <= Applicative f where
    pure :: forall a. a -> f a

  liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
  liftA1 f a = pure f <*> a

  infixl 1 >>=

  -- | A `Bind` is an [`Apply`](#apply) with a bind operation which sequentially composes actions.
  -- |
  -- | `Bind`s should obey the following rule.
  -- |
  -- | Associativity:
  -- |     `forall f g x. (x >>= f) >>= g = x >>= (\k => f k >>= g)`
  -- |
  class (Apply m) <= Bind m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  -- | `Monad` is a class which can be intuitively thought of as an abstract datatype of actions or
  -- | more formally though of as a monoid in the category of endofunctors.
  -- |
  -- | `Monad`s should obey the following rules.
  -- |
  -- | Left Identity:
  -- |     `forall f x. pure x >>= f = f x`
  -- |
  -- | Right Identity:
  -- |     `forall x. x >>= pure = x`
  -- |
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

  -- | Addition and multiplication
  class Semiring a where
    (+)  :: a -> a -> a
    zero :: a
    (*)  :: a -> a -> a
    one  :: a

  -- | Semiring with modulo operation and division where
  -- | ```a / b * b + (a `mod` b) = a```
  class (Semiring a) <= ModuloSemiring a where
    (/) :: a -> a -> a
    mod :: a -> a -> a

  -- | Addition, multiplication, and subtraction
  class (Semiring a) <= Ring a where
    (-) :: a -> a -> a

  negate :: forall a. (Ring a) => a -> a
  negate a = zero - a

  -- | Ring where every nonzero element has a multiplicative inverse (possibly
  -- | a non-commutative field) so that ```a `mod` b = zero```
  class (Ring a, ModuloSemiring a) <= DivisionRing a

  -- | A commutative field
  class (DivisionRing a) <= Num a

  foreign import binary_add_operator
    """
    template <typename T>
    inline auto binary_add_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 + n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import binary_sub_operator
    """
    template <typename T>
    inline auto binary_sub_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 - n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import binary_mul_operator
    """
    template <typename T>
    inline auto binary_mul_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 * n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import binary_div_operator
    """
    template <typename T>
    inline auto binary_div_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 / n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import binary_mod_operator
    """
    template <typename T>
    inline auto binary_mod_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 % n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import unary_neg_operator
    """
    template <typename T>
    inline auto unary_neg_operator(T n) -> T {
      return -n;
    }
    """ :: forall a. a -> a
  (%) = binary_mod_operator

  instance semiringNumber :: Semiring Number where
    (+) = binary_add_operator
    zero = 0
    (*) = binary_mul_operator
    one = 1

  instance ringNumber :: Ring Number where
    (-) = binary_sub_operator

  instance moduloSemiringNumber :: ModuloSemiring Number where
    (/) = binary_div_operator
    mod _ _ = 0

  instance divisionRingNumber :: DivisionRing Number

  instance numNumber :: Num Number

  newtype Unit = Unit {}

  unit :: Unit
  unit = Unit {}

  infix 4 ==
  infix 4 /=

  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

  foreign import binary_eq_operator
    """
    template <typename T>
    inline auto binary_eq_operator(T r1) -> fn<T,bool> {
      return [=](T r2) {
        return r1 == r2;
      };
    }
    """ :: forall a. a -> a -> Boolean

  foreign import binary_neq_operator
    """
    template <typename T>
    inline auto binary_neq_operator(T r1) -> fn<T,bool> {
      return [=](T r2) {
        return r1 != r2;
      };
    }
    """ :: forall a. a -> a -> Boolean

  instance eqUnit :: Eq Unit where
    (==) (Unit {}) (Unit {}) = true
    (/=) (Unit {}) (Unit {}) = false

  instance eqString :: Eq String where
    (==) = binary_eq_operator
    (/=) = binary_neq_operator

  instance eqNumber :: Eq Number where
    (==) = binary_eq_operator
    (/=) = binary_neq_operator

  instance eqBoolean :: Eq Boolean where
    (==) = binary_eq_operator
    (/=) = binary_neq_operator

  instance eqArray :: (Eq a) => Eq [a] where
    (==) = binary_eq_operator
    (/=) = binary_neq_operator

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

  instance semigroupOrdening :: Semigroup Ordering where
    (<>) LT _ = LT
    (<>) GT _ = GT
    (<>) EQ y = y


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

  foreign import unsafeCompareImpl
    """
    template <typename T>
    inline auto unsafeCompareImpl(data<Ordering> lt) -> fn<data<Ordering>,fn<data<Ordering>,fn<T,fn<T,data<Ordering>>>>> {
      return [=](data<Ordering> eq) -> fn<data<Ordering>,fn<T,fn<T,data<Ordering>>>> {
        return [=](data<Ordering> gt) -> fn<T,fn<T,data<Ordering>>> {
          return [=](T x) -> fn<T,data<Ordering>> {
            return [=](T y) -> data<Ordering> {
              return x < y ? lt : x > y ? gt : eq;
            };
          };
        };
      };
    }
    """ :: forall a. Ordering -> Ordering -> Ordering -> a -> a -> Ordering

  unsafeCompare :: forall a. a -> a -> Ordering
  unsafeCompare = unsafeCompareImpl LT EQ GT

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

  foreign import binary_shl_operator
    """
    template <typename T>
    inline auto binary_shl_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 << n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import binary_shr_operator
    """
    template <typename T>
    inline auto binary_shr_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 >> n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import binary_bitand_operator
    """
    template <typename T>
    inline auto binary_bitand_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 & n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import binary_bitor_operator
    """
    template <typename T>
    inline auto binary_bitor_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 | n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import binary_bitxor_operator
    """
    template <typename T>
    inline auto binary_bitxor_operator(T n1) -> fn<T,T> {
      return [=](T n2) {
        return n1 ^ n2;
      };
    }
    """ :: forall a. a -> a -> a

  foreign import unary_comp_operator
    """
    template <typename T>
    inline auto unary_comp_operator(T n) -> T {
      return ~n;
    }
    """ :: forall a. a -> a

  instance bitsNumber :: Bits Number where
    (.&.) = binary_bitand_operator
    (.|.) = binary_bitor_operator
    (.^.) = binary_bitxor_operator
    shl = binary_shl_operator
    shr = binary_shr_operator
    zshr = binary_shr_operator
    complement = unary_comp_operator

  infixr 2 ||
  infixr 3 &&

  class BoolLike b where
    (&&) :: b -> b -> b
    (||) :: b -> b -> b
    not :: b -> b

  foreign import binary_and_operator
    """
    template <typename T>
    inline auto binary_and_operator(T r1) -> fn<T,bool> {
      return [=](T r2) {
        return r1 && r2;
      };
    }
    """ :: forall a. a -> a -> Boolean

  foreign import binary_or_operator
    """
    template <typename T>
    inline auto binary_or_operator(T r1) -> fn<T,bool> {
      return [=](T r2) {
        return r1 || r2;
      };
    }
    """ :: forall a. a -> a -> Boolean

  foreign import unary_not_operator
    """
    template <typename T>
    inline auto unary_not_operator(T n) -> bool {
      return !n;
    }
    """ :: forall a. a -> Boolean

  instance boolLikeBoolean :: BoolLike Boolean where
    (&&) = binary_and_operator
    (||) = binary_or_operator
    not = unary_not_operator

  infixr 5 <>

  class Semigroup a where
    (<>) :: a -> a -> a

  instance semigroupUnit :: Semigroup Unit where
    (<>) (Unit {}) (Unit {}) = Unit {}

  instance semigroupString :: Semigroup String where
    (<>) = binary_add_operator

  instance semigroupArr :: (Semigroup s') => Semigroup (s -> s') where
    (<>) f g = \x -> f x <> g x

  infixr 5 ++

  (++) :: forall s. (Semigroup s) => s -> s -> s
  (++) = (<>)

{-
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

  foreign import mkFn0
    """
    function mkFn0(fn) {
      return function() {
        return fn({});
      };
    }
    """ :: forall a. (Unit -> a) -> Fn0 a

  foreign import mkFn1
    """
    function mkFn1(fn) {
      return function(a) {
        return fn(a);
      };
    }
    """ :: forall a b. (a -> b) -> Fn1 a b

  foreign import mkFn2
    """
    function mkFn2(fn) {
      return function(a, b) {
        return fn(a)(b);
      };
    }
    """ :: forall a b c. (a -> b -> c) -> Fn2 a b c

  foreign import mkFn3
    """
    function mkFn3(fn) {
      return function(a, b, c) {
        return fn(a)(b)(c);
      };
    }
    """ :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d

  foreign import mkFn4
    """
    function mkFn4(fn) {
      return function(a, b, c, d) {
        return fn(a)(b)(c)(d);
      };
    }
    """ :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e

  foreign import mkFn5
    """
    function mkFn5(fn) {
      return function(a, b, c, d, e) {
        return fn(a)(b)(c)(d)(e);
      };
    }
    """ :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f

  foreign import mkFn6
    """
    function mkFn6(fn) {
      return function(a, b, c, d, e, f) {
        return fn(a)(b)(c)(d)(e)(f);
      };
    }
    """ :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g

  foreign import mkFn7
    """
    function mkFn7(fn) {
      return function(a, b, c, d, e, f, g) {
        return fn(a)(b)(c)(d)(e)(f)(g);
      };
    }
    """ :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h

  foreign import mkFn8
    """
    function mkFn8(fn) {
      return function(a, b, c, d, e, f, g, h) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h);
      };
    }
    """ :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i

  foreign import mkFn9
    """
    function mkFn9(fn) {
      return function(a, b, c, d, e, f, g, h, i) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
      };
    }
    """ :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j

  foreign import mkFn10
    """
    function mkFn10(fn) {
      return function(a, b, c, d, e, f, g, h, i, j) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
      };
    }
    """ :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k

  foreign import runFn0
    """
    function runFn0(fn) {
      return fn();
    }
    """ :: forall a. Fn0 a -> a

  foreign import runFn1
    """
    function runFn1(fn) {
      return function(a) {
        return fn(a);
      };
    }
    """ :: forall a b. Fn1 a b -> a -> b

  foreign import runFn2
    """
    function runFn2(fn) {
      return function(a) {
        return function(b) {
          return fn(a, b);
        };
      };
    }
    """ :: forall a b c. Fn2 a b c -> a -> b -> c

  foreign import runFn3
    """
    function runFn3(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return fn(a, b, c);
          };
        };
      };
    }
    """ :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d

  foreign import runFn4
    """
    function runFn4(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return fn(a, b, c, d);
            };
          };
        };
      };
    }
    """ :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e

  foreign import runFn5
    """
    function runFn5(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return fn(a, b, c, d, e);
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f

  foreign import runFn6
    """
    function runFn6(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return fn(a, b, c, d, e, f);
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g. Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g

  foreign import runFn7
    """
    function runFn7(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return fn(a, b, c, d, e, f, g);
                  };
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g h. Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h

  foreign import runFn8
    """
    function runFn8(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return fn(a, b, c, d, e, f, g, h);
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g h i. Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i

  foreign import runFn9
    """
    function runFn9(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return function(i) {
                        return fn(a, b, c, d, e, f, g, h, i);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g h i j. Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j

  foreign import runFn10
    """
    function runFn10(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return function(i) {
                        return function(j) {
                          return fn(a, b, c, d, e, f, g, h, i, j);
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g h i j k. Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k
-}

module Prelude.Unsafe where

  foreign import unsafeIndex
    """
    template <typename T>
    inline auto unsafeIndex(list<T> xs) -> fn<list_index_type,T> {
      return [=](list_index_type n) {
        return xs[n]; // consider using at() instead, which does bounds checking
      };
    }
    """ :: forall a. [a] -> Number -> a

module Control.Monad.Eff where

  foreign import data Eff :: # ! -> * -> *

  foreign import returnE
    """
    template <typename A>
    inline auto returnE(A a) -> eff_fn<A> {
      return [=]() {
        return a;
      };
    }
    """ :: forall e a. a -> Eff e a

  foreign import bindE
    """
    template <typename A, typename B>
    inline auto bindE(eff_fn<A> a) -> fn<fn<A,eff_fn<B>>,eff_fn<B>> {
      return [=](fn<A,eff_fn<B>> f) {
        return [=]() {
          return f(a())();
        };
      };
    }
    """ :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

  -- TODO: can we get this info and use to create type aliases on the C++ side?
  type Pure a = forall e. Eff e a

  foreign import runPure
    """
    template <typename A>
    inline auto runPure(eff_fn<A> f) -> A {
      return f();
    }
    """ :: forall a. Pure a -> a

  instance functorEff :: Functor (Eff e) where
    (<$>) = liftA1

  instance applyEff :: Apply (Eff e) where
    (<*>) = ap

  instance applicativeEff :: Applicative (Eff e) where
    pure = returnE

  instance bindEff :: Bind (Eff e) where
    (>>=) = bindE

  instance monadEff :: Monad (Eff e)

  foreign import untilE
    """
    inline auto untilE(eff_fn<bool> f) -> eff_fn<data<Prelude::Unit>> {
      return [=]() {
        while (!f());
        return Prelude::unit;
      };
    }
    """ :: forall e. Eff e Boolean -> Eff e Unit

  foreign import whileE
    """
    template <typename A>
    inline auto whileE(eff_fn<bool> f) -> fn<eff_fn<A>,eff_fn<Prelude::Unit>> {
      return [=](eff_fn<A> a) {
        return [=]() {
          while (f()) {
            a();
          }
          return Prelude::unit;
        };
      };
    }
    """ :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit


  foreign import forE
    """
    inline auto forE(long lo) -> fn<long,fn<fn<long,eff_fn<Prelude::Unit>>,eff_fn<data<Prelude::Unit>>>> {
      return [=](long hi) {
        return [=](fn<long,eff_fn<Prelude::Unit>> f) {
          return [=]() {
            for (auto i = lo; i < hi; i++) {
              f(i)();
            }
            return Prelude::unit;
          };
        };
      };
    }
    """ :: forall e. Number -> Number -> (Number -> Eff e Unit) -> Eff e Unit

  foreign import foreachE
    """
    template <typename A>
    inline auto foreachE(list<A> as) -> fn<fn<A,eff_fn<Prelude::Unit>>,eff_fn<data<Prelude::Unit>>> {
      return [=](fn<A,eff_fn<Prelude::Unit>> f) {
        return [=]() {
          for (auto i = 0; i < as.size(); i++) {
            f(as[i])();
          }
          return Prelude::unit;
        };
      };
    }
    """ :: forall e a. [a] -> (a -> Eff e Unit) -> Eff e Unit

module Control.Monad.Eff.Unsafe where

  import Control.Monad.Eff

  foreign import unsafeInterleaveEff
    """
    template <typename A>
    inline auto unsafeInterleaveEff(eff_fn<A> f) -> eff_fn<A> {
      return f;
    }
    """ :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a

module Debug.Trace where

  import Control.Monad.Eff

  foreign import data Trace :: !

  foreign import trace
    """
    inline auto trace(string s) -> eff_fn<data<Prelude::Unit>> {
      return [=]() {
        std::cout << s << std::endl;
        return Prelude::unit;
      };
    }
    """ :: forall r. String -> Eff (trace :: Trace | r) Unit

  print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) Unit
  print o = trace (show o)

{-

module Control.Monad.ST where

  import Control.Monad.Eff

  foreign import data ST :: * -> !

  foreign import data STRef :: * -> * -> *

  foreign import newSTRef
    """
    function newSTRef(val) {
      return function() {
        return { value: val };
      };
    }
    """ :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

  foreign import readSTRef
    """
    function readSTRef(ref) {
      return function() {
        return ref.value;
      };
    }
    """ :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

  foreign import modifySTRef
    """
    function modifySTRef(ref) {
      return function(f) {
        return function() {
          return ref.value = f(ref.value);
        };
      };
    }
    """ :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a

  foreign import writeSTRef
    """
    function writeSTRef(ref) {
      return function(a) {
        return function() {
          return ref.value = a;
        };
      };
    }
    """ :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) a

  foreign import runST
    """
    function runST(f) {
      return f;
    }
    """ :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a

  pureST :: forall a. (forall h r. Eff (st :: ST h | r) a) -> a
  pureST st = runPure (runST st)

-}
