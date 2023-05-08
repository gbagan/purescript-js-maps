module JS.Map.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Lens (lens)
import Data.Lens.AffineTraversal (affineTraversal)
import Data.Lens.At (class At)
import Data.Lens.Index (class Index)
import Data.Maybe (Maybe, maybe, maybe')
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested ((/\))
import JS.Map.EncodeKey (class EncodeKey, encodeKey)
import JS.Map.Primitive as P

newtype Map k v = Map (P.Map String (Tuple k v))

fromArray :: forall k v. EncodeKey k => Array (Tuple k v) -> Map k v
fromArray xs = xs
  <#> (\kv -> encodeKey (fst kv) /\ kv)
  # P.fromFoldable
  # Map

toArray :: forall k v. Map k v -> Array (Tuple k v)
toArray (Map m) = P.values m

empty :: forall k v. Map k v
empty = Map $ P.empty

isEmpty :: forall k v. Map k v -> Boolean
isEmpty (Map m) = P.isEmpty m

size :: forall k v. Map k v -> Int
size (Map m) = P.size m

member :: forall k v. EncodeKey k => k -> Map k v -> Boolean
member key (Map obj) = P.member (encodeKey key) obj

insert :: forall k v. EncodeKey k => k -> v -> Map k v -> Map k v
insert key val (Map obj) = Map $
  P.insert (encodeKey key) (Tuple key val) obj

delete :: forall k v. EncodeKey k => k -> Map k v -> Map k v
delete key (Map obj) = Map $ P.delete (encodeKey key) obj

lookup :: forall k v. EncodeKey k => k -> Map k v -> Maybe v
lookup key (Map obj) = P.lookup (encodeKey key) obj <#> snd

-- | Insert, remove or update a value for a key in a map
alter :: forall k v. EncodeKey k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f key (Map obj) = Map $ P.alter f' (encodeKey key) obj
  where
  f' v = (key /\ _) <$> f (snd <$> v)

-- | Remove or update a value for a key in a map
update :: forall k v. EncodeKey k => (v -> Maybe v) -> k -> Map k v -> Map k v
update f key (Map obj) = Map $ P.update f' (encodeKey key) obj
  where
  f' v = (key /\ _) <$> f (snd v)

instance EncodeKey k => Functor (Map k) where
  map f (Map m) = Map $ m <#> \(k /\ v) -> k /\ f v

instance EncodeKey k => FunctorWithIndex k (Map k) where
  mapWithIndex f (Map m) = Map $ m <#> \(k /\ v) -> k /\ f k v

instance EncodeKey k => Foldable (Map k) where
  foldl f z (Map obj) = foldl (\y v -> f y (snd v)) z obj
  foldr f z (Map obj) = foldr (\v y -> f (snd v) y) z obj
  foldMap f (Map obj) = foldMap (f <<< snd) obj

instance EncodeKey k => FoldableWithIndex k (Map k) where
  foldlWithIndex f z (Map obj) = foldl (\y (k /\ v) -> f k y v) z obj
  foldrWithIndex f z (Map obj) = foldr (\(k /\ v) y -> f k v y) z obj
  foldMapWithIndex f (Map obj) = foldMap (uncurry f) obj

instance EncodeKey k => Traversable (Map k) where
  traverse = traverseWithIndex <<< const
  sequence = traverse identity

instance EncodeKey k => TraversableWithIndex k (Map k) where
  traverseWithIndex f (Map obj) = Map <$> traverse (\(k /\ v) -> (k /\ _) <$> f k v) obj

instance EncodeKey k => Index (Map k v) k v where
  ix k = affineTraversal set pre
    where
    set :: Map k v -> v -> Map k v
    set s b = insert k b s

    pre :: Map k v -> Either (Map k v) v
    pre s = maybe (Left s) Right $ lookup k s

instance EncodeKey k => At (Map k v) k v where
  at k =
    lens (lookup k) \m ->
      maybe' (\_ -> delete k m) \v -> insert k v m
