-- @inline export size arity=1
-- @inline export peek arity=2
-- @inline export poke arity=3
-- @inline export delete arity=2
module JS.Map.Primitive.ST
  ( size
  , delete
  , module I
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Uncurried (STFn1, runSTFn1)
import JS.Map.Primitive.Key (class Key)
import JS.Map.Primitive.Internal (STMap, new, peek, poke, poke_, delete_, thaw, freeze, run, mutate) as I
import JS.Map.Primitive.Internal (STMap, deleteST)


size :: forall r k v. STMap r k v -> ST r Int
size = runSTFn1 sizeImpl

delete :: forall r k v. Key k => k -> STMap r k v -> ST r (STMap r k v)
delete = deleteST

foreign import sizeImpl :: forall r m. STFn1 m r Int