module Data.Set.Monad 
  ( Set
  , run
  , fromFoldable
  , toUnfoldable
  , empty
  , isEmpty
  , singleton
  , map
  , insert
  , member, notMember
  , delete
  , size
  , union
  , difference, (\\)
  , subset
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Foldable (foldl) as F
import Data.List (List)
import Data.List (toUnfoldable) as List
import Data.Set as S
import Data.Unfoldable (class Unfoldable)

-- data Set a where
--   Prim   :: (Ord a) => S.Set a -> Set a
--   Return :: a -> Set a
--   Bind   :: Set a -> (a -> Set b) -> Set b
--   Zero   :: Set a
--   Plus   :: Set a -> Set a -> Set a

-- Encoding the Haskell GADT as a sigma type https://stackoverflow.com/questions/13653532/how-to-express-existential-types-using-higher-rank-rank-n-type-polymorphism

data Set a
  = Prim (forall r. (Ord a => S.Set a -> r) -> r)
  | Return (a)
  | Bind (forall r. (forall b. Set b -> (b -> Set a) -> r) -> r)
  | Zero   
  | Plus   (Set a) (Set a)

run :: forall a. (Ord a) => Set a -> S.Set a
run (Prim s)     = s identity
run (Return a)   = S.singleton a
run (Zero)       = S.empty
run (Plus ma mb) = run ma `S.union` run mb
run (Bind e)     = e \x f -> 
  case x of
    Prim s               -> F.foldl S.union S.empty (S.map (run <<< f) $ s identity)
    Return a             -> run (f a)
    Zero                 -> S.empty
    Plus (Prim s) ma     -> run $ bind' (s \z -> prim $ S.union z (run ma)) f
    Plus ma (Prim s)     -> run $ bind' (s \z -> prim $ S.union (run ma) z) f
    Plus (Return a) ma   -> run (Plus (f a) (bind' ma f))
    Plus ma (Return a)   -> run (Plus (bind' ma f) (f a))
    Plus Zero ma         -> run (bind' ma f)
    Plus ma Zero         -> run (bind' ma f)
    Plus (Plus ma mb) mc -> run (bind' (Plus ma (Plus mb mc)) f)
    Plus ma mb           -> run (Plus (bind' ma f) (bind' mb f))
    Bind e'              -> e' \ma g -> run (bind' ma (\a -> bind' (g a) f))

prim :: forall a. Ord a => S.Set a -> Set a
prim a = Prim (_ $ a)

bind' :: forall a b. Set b -> (b -> Set a) -> Set a
bind' b f = Bind (\x -> x b f)

instance Functor Set where
  map = liftM1

instance Apply Set where
  apply = ap

instance Applicative Set where
  pure  = Return

instance Bind Set where
  bind x f = bind' x f

instance Monad Set 

instance Semigroup (Set a) where
  append = Plus

instance (Ord a) => Monoid (Set a) where
  mempty  = empty

instance (Show a, Ord a) => Show (Set a) where
  show = show <<< run

toUnfoldable :: forall f a. Unfoldable f => Ord a => Set a -> f a
toUnfoldable = List.toUnfoldable <<< toList

fromFoldable :: forall f a. Foldable f => Ord a => f a -> Set a
fromFoldable = F.foldl (\m a -> insert a m) empty

toList :: forall a. (Ord a) => Set a -> List a
toList = S.toUnfoldable <<< run

difference :: forall a. (Ord a) => Set a -> Set a -> Set a
difference m1 m2 = difference m1 m2

infixr 6 difference as \\

isEmpty :: forall a. (Ord a) => Set a -> Boolean
isEmpty = S.isEmpty <<< run

size :: forall a. (Ord a) => Set a -> Int
size = S.size <<< run

member :: forall a. (Ord a) => a -> Set a -> Boolean
member a s = S.member a (run s)

notMember :: forall a. (Ord a) => a -> Set a -> Boolean
notMember a t = not (member a t)

subset :: forall a. Ord a => Set a -> Set a -> Boolean
subset s1 s2 = S.subset (run s1) (run s2)

empty :: forall a. (Ord a) => Set a
empty = prim S.empty

union :: forall a. (Ord a) => Set a -> Set a -> Set a
union s1 s2 = prim (run s1 `S.union` run s2)

singleton :: forall a. (Ord a) => a -> Set a
singleton a = prim (S.singleton a)

-- | Maps over the values in a set.
-- |
-- | This operation is not structure-preserving for sets, so is not a valid
-- | `Functor`. An example case: mapping `const x` over a set with `n > 0`
-- | elements will result in a set with one element.
map :: forall a b. Ord a => Ord b => (a -> b) -> Set a -> Set b
map f = foldl (\m a -> insert (f a) m) empty

insert :: forall a. (Ord a) => a -> Set a -> Set a
insert a s = prim (S.insert a (run s))

delete :: forall a. (Ord a) => a -> Set a -> Set a
delete a s = prim (S.delete a (run s))

foldl :: forall a b. (Ord a) => (b -> a -> b) -> b -> Set a -> b
foldl f z s = F.foldl f z (run s)