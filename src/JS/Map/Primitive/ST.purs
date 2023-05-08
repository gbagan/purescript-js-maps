-- @inline export size arity=1
-- @inline export peek arity=2
-- @inline export poke arity=3
-- @inline export delete arity=2
module JS.Map.Primitive.ST
  ( STMap
  , new
  , size
  , peek
  , poke
  , poke_
  , delete
  , delete_
  ) where

import Prelude

import Control.Monad.ST (ST, Region)
import Control.Monad.ST.Uncurried (STFn1, STFn2, STFn3, STFn4, runSTFn1, runSTFn2, runSTFn3, runSTFn4)
import Data.Maybe (Maybe(..))
import JS.Map.Primitive.Key (class Key)

-- | A reference to a mutable Map
-- |
-- | The first type parameter represents the memory region which the map belongs
-- | to. The second type parameter defines the type of elements of the mutable
-- | map.
-- |
-- | The runtime representation of a value of type `STMap r k v` is the same as
-- | that of `Map k v`, except that mutation is allowed.
foreign import data STMap :: Region -> Type -> Type -> Type

-- | Create a new, empty mutable map
foreign import new :: forall r k v. ST r (STMap r k v)


size :: forall r k v. STMap r k v -> ST r Int
size = runSTFn1 sizeImpl

foreign import sizeImpl :: forall r m. STFn1 m r Int

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

delete :: forall r k v. Key k => k -> STMap r k v -> ST r (STMap r k v)
delete k m = do
  runSTFn2 deleteImpl m k
  pure m

-- | Remove a key and the corresponding value from a mutable map
delete_ :: forall r k v. Key k => k -> STMap r k v -> ST r Unit
delete_ k m = runSTFn2 deleteImpl m k

foreign import deleteImpl :: forall r k m. STFn2 m k r Unit