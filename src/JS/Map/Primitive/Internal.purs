module JS.Map.Primitive.Internal where

import Prelude

import Control.Monad.ST (ST, Region)
import Control.Monad.ST as ST
import Control.Monad.ST.Uncurried (STFn2, STFn3, STFn4, runSTFn2, runSTFn3, runSTFn4)
import Data.Array as A
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr, for_)
import Data.FoldableWithIndex (class FoldableWithIndex, forWithIndex_)
import Data.Function.Uncurried (Fn2, runFn2, Fn4, runFn4)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Unfoldable (class Unfoldable)
import JS.Map.Primitive.Key (class Key)

foreign import data Map :: Type -> Type -> Type

type role Map nominal representational

-- | A reference to a mutable Map
-- |
-- | The first type parameter represents the memory region which the map belongs
-- | to. The second type parameter defines the type of elements of the mutable
-- | map.
-- |
-- | The runtime representation of a value of type `STMap r k v` is the same as
-- | that of `Map k v`, except that mutation is allowed.
foreign import data STMap :: Region -> Type -> Type -> Type

type role STMap nominal nominal representational

foreign import _copyST :: forall a b r. a -> ST r b

-- | Convert an immutable Map into a mutable Map
thaw :: forall r k v. Map k v -> ST r (STMap r k v)
thaw = _copyST

-- | Convert a mutable Map into an immutable Map
freeze :: forall r k v. STMap r k v -> ST r (Map  k v)
freeze = _copyST

-- | Freeze a mutable Map, creating an immutable Map. Use this function as you would use
-- | `Control.Monad.ST.run` (from the `purescript-st` package) to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the Map from escaping the scope of `run`.
foreign import run :: forall k v. (forall r. ST r (STMap r k v)) -> Map k v

mutate :: forall a k v. Key k => (forall r. STMap r k v -> ST r a) -> Map k v -> Map k v
mutate f m = run do
  s <- thaw m
  _ <- f s
  pure s

foreign import _fmapMap :: forall k v v'. Fn2 (Map k v) (v -> v') (Map k v')

instance Key k => Functor (Map k) where
  map f m = runFn2 _fmapMap m f

instance Key k => FunctorWithIndex k (Map k) where
  mapWithIndex = mapWithKey

foreign import _foldM :: forall k v m z. (m -> (z -> m) -> m) -> (z -> k -> v -> m) -> m -> Map k v -> m

-- | Fold the keys and values of an object
fold :: forall k v z. Key k => (z -> k -> v -> z) -> z -> Map  k v -> z
fold = _foldM ((#))

-- | Fold the keys and values of an object, accumulating values using some
-- | `Monoid`.
foldMap :: forall k v m. Key k => Monoid m => (k -> v -> m) -> Map k v -> m
foldMap f = fold (\acc k v -> acc <> f k v) mempty

-- | Fold the keys and values of an object, accumulating values and effects in
-- | some `Monad`.
foldM :: forall k v m z. Key k => Monad m => (z -> k -> v -> m z) -> z -> Map k v -> m z
foldM f z = _foldM bind f (pure z)

instance Key k =>  Foldable (Map k) where
  foldl f = fold (\z _ -> f z)
  foldr f z m = foldr f z (values m)
  foldMap f = foldMap (const f)

instance Key k => FoldableWithIndex k (Map k) where
  foldlWithIndex f = fold (flip f)
  foldrWithIndex f z m = foldr (uncurry f) z (toArrayWithKey Tuple m)
  foldMapWithIndex = foldMap

instance Key k => Traversable (Map k) where
  traverse = traverseWithIndex <<< const
  sequence = traverse identity

instance Key k => TraversableWithIndex k (Map k) where
  traverseWithIndex f ms =
    fold (\acc k v -> flip (insert k) <$> acc <*> f k v) (pure empty) ms

-- Unfortunately the above are not short-circuitable (consider using purescript-machines)
-- so we need special cases:

foreign import _foldSCMap :: forall k v z. Fn4 (Map k v) z (z -> k -> v -> Maybe z) (forall b. b -> Maybe b -> b) z

-- | Fold the keys and values of a map.
-- |
-- | This function allows the folding function to terminate the fold early,
-- | using `Maybe`.
foldMaybe :: forall k v z. Key k => (z -> k -> v -> Maybe z) -> z -> Map k v -> z
foldMaybe f z m = runFn4 _foldSCMap m z f fromMaybe

-- | Test whether all key/value pairs in a `Map` satisfy a predicate.
all :: forall k v. Key k => (k -> v -> Boolean) -> Map k v -> Boolean
all = _all

foreign import _all :: forall k v. (k -> v -> Boolean) -> Map k v -> Boolean

instance (Key k, Eq v) => Eq (Map k v) where
  eq m1 m2 = (isSubmap m1 m2) && (isSubmap m2 m1)

instance  Key k => Eq1 (Map k) where
  eq1 = eq

-- Internal use
toAscArray :: forall k v. Key k => Map k v -> Array (Tuple k v)
toAscArray = toAscUnfoldable

instance (Key k, Ord v) => Ord (Map k v) where
  compare m1 m2 = compare (toAscArray m1) (toAscArray m2)

instance (Key k, Show v) => Show (Map k v) where
  show m = "(fromFoldable " <> show (toArray m) <> ")"

-- | An empty map
foreign import empty :: forall k v. Map k v

-- | Test whether one map contains all of the keys and values contained in another map
isSubmap :: forall k v. Key k => Eq v => Map k v -> Map k v -> Boolean
isSubmap m1 m2 = all f m1 where
  f k v = runFn4 _lookup false ((==) v) k m2

-- | Test whether a map is empty
isEmpty :: forall k v. Key k => Map k v -> Boolean
isEmpty = all (\_ _ -> false)

-- | Calculate the number of key/value pairs in a map
foreign import size :: forall k v. Map k v -> Int

-- | Create an `Map a` with one key/value pair
singleton :: forall k v. Key k => k -> v -> Map  k v
singleton k v = run (poke k v =<< new)

foreign import _lookup :: forall k v z. Fn4 z (v -> z) k (Map k v) z

-- | Lookup the value for a key in a map
lookup :: forall k v. Key k => k -> Map k v -> Maybe v
lookup = runFn4 _lookup Nothing Just

-- | Test whether a key appears in a map
member :: forall k v. Key k => k -> Map k v -> Boolean
member = runFn4 _lookup false (const true)

-- | Insert or replace a key/value pair in a map
insert :: forall k v. Key k => k -> v -> Map k v -> Map k v
insert k v = mutate (poke k v)

-- | Delete a key and value from a map
delete :: forall k v. Key k => k -> Map k v -> Map k v
delete k = mutate (deleteST k)

-- | Delete a key and value from a map, returning the value
-- | as well as the subsequent map
pop :: forall k v. Key k => k -> Map k v -> Maybe (Tuple v (Map k v))
pop k m = lookup k m <#> \a -> Tuple a (delete k m)

-- | Insert, remove or update a value for a key in a map
alter :: forall k v. Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Remove or update a value for a key in a map
update :: forall k v. Key k => (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = alter (maybe Nothing f) k m

-- | Create an `Map k v` from a foldable collection of key/value pairs
fromFoldable :: forall f k v. Key k => Foldable f => f (Tuple k v) -> Map k v
fromFoldable l = run do
  s <- new
  ST.foreach (A.fromFoldable l) \(Tuple k v) -> void $ poke k v s
  pure s

-- | Create an `Map k v` from a k-indexed foldable collection
fromFoldableWithIndex :: forall f k v. Key k => FoldableWithIndex k (f k) => f k v -> Map k v
fromFoldableWithIndex l = run do
  s <- new
  forWithIndex_ l \k v -> poke k v s
  pure s

foreign import _lookupST :: forall k v r z. STFn4 z (v -> z) k (STMap r k v) r z

-- | Create an `Map k v` from a foldable collection of key/value pairs, using the
-- | specified function to combine values for duplicate keys.
fromFoldableWith :: forall f k v. Key k => Foldable f => (v -> v -> v) -> f (Tuple k v) -> Map k v
fromFoldableWith f l = run (do
  s <- new
  for_ l \(Tuple k v) ->
    runSTFn4 _lookupST v (f v) k s >>= \v' -> poke k v' s
  pure s)

foreign import toArrayWithKey :: forall k v a. (k -> v -> a) -> Map k v -> Array a

-- | Unfolds a map into a list of key/value pairs
toUnfoldable :: forall f k v. Key k => Unfoldable f => Map k v -> f (Tuple k v)
toUnfoldable = A.toUnfoldable <<< toArrayWithKey Tuple

-- | Unfolds a map into a list of key/value pairs which is guaranteed to be
-- | sorted by key
toAscUnfoldable :: forall f k v. Key k => Unfoldable f => Map k v -> f (Tuple k v)
toAscUnfoldable = A.toUnfoldable <<< A.sortWith fst <<< toArrayWithKey Tuple

-- Internal
toArray :: forall k v. Key k => Map k v -> Array (Tuple k v)
toArray = toArrayWithKey Tuple

-- | Get an array of the keys in a map
foreign import keys :: forall k v. Map k v -> Array k

-- | Get a list of the values in a map
values :: forall k v. Map k v -> Array v
values = toArrayWithKey (\_ v -> v)

-- | Compute the union of two maps, preferring the first map in the case of
-- | duplicate keys.
union :: forall k v. Key k => Map k v -> Map k v -> Map k v
union m = mutate (\s -> foldM (\s' k v -> poke k v s') s m)

-- | Compute the union of two maps, using the specified function
-- | to combine values for duplicate keys.
unionWith :: forall k v. Key k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f m1 m2 =
  mutate (\s1 -> foldM (\s2 k v1 -> poke k (runFn4 _lookup v1 (\v2 -> f v1 v2) k m2) s2) s1 m1) m2

-- | Compute the union of a collection of maps
unions :: forall f k v. Key k => Foldable f => f (Map k v) -> Map k v
unions = foldl union empty

foreign import _mapWithKey :: forall k v v'. Fn2 (Map k v) (k -> v -> v') (Map k v')

-- | Apply a function of two arguments to each key/value pair, producing a new map
mapWithKey :: forall k v v'. Key k => (k -> v -> v') -> Map k v -> Map k v'
mapWithKey f m = runFn2 _mapWithKey m f

instance (Key k, Semigroup v) => Semigroup (Map k v) where
  append = unionWith (<>)

instance (Key k, Semigroup v) => Monoid (Map k v) where
  mempty = empty

-- | Filter out those key/value pairs of a map for which a predicate
-- | fails to hold.
filterWithKey :: forall k v. Key k => (k -> v -> Boolean) -> Map k v -> Map k v
filterWithKey predicate m = run go
  where
  go :: forall r. ST r (STMap r k v)
  go = do
    m' <- new
    foldM step m' m
    where
      step acc k v = if predicate k v then poke k v acc else pure acc

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the key fails to hold.
filterKeys :: forall k. Key k => (k -> Boolean) -> Map k ~> Map k
filterKeys predicate = filterWithKey $ const <<< predicate

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the value fails to hold.
filter :: forall k v. Key k => (v -> Boolean) -> Map k v -> Map k v
filter predicate = filterWithKey $ const predicate

-- | Create a new, empty mutable map
foreign import new :: forall r k v. ST r (STMap r k v)

-- | Get the value for a key in a mutable map
peek :: forall r k v. Key k => k -> STMap r k v -> ST r (Maybe v)
peek k m = runSTFn4 peekImpl m k Just Nothing

foreign import peekImpl :: forall r m k v. STFn4 m k (v -> Maybe v) (Maybe v) r v

-- | Update the value for a key in a mutable map
poke :: forall r k v. Key k => k -> v -> STMap r k v -> ST r (STMap r k v)
poke k v m = do
  runSTFn3 pokeImpl m k v
  pure m

poke_ :: forall r k v. Key k => k -> v -> STMap r k v -> ST r Unit
poke_ k v m = runSTFn3 pokeImpl m k v

foreign import pokeImpl :: forall r k v m. STFn3 m k v r Unit

deleteST :: forall r k v. Key k => k -> STMap r k v -> ST r (STMap r k v)
deleteST k m = do
  runSTFn2 deleteImpl m k
  pure m

-- | Remove a key and the corresponding value from a mutable map
delete_ :: forall r k v. Key k => k -> STMap r k v -> ST r Unit
delete_ k m = runSTFn2 deleteImpl m k

foreign import deleteImpl :: forall r k m. STFn2 m k r Unit