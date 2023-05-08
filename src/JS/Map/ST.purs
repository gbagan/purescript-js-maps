module JS.Map.ST
  ( STMap
  , delete
  , delete_
  , freeze
  , new
  , peek
  , poke
  , poke_
  , modify
  , run
  , thaw
  -- , unsafeFreeze
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\))
import JS.Map.EncodeKey (class EncodeKey, encodeKey)
import JS.Map.Internal (Map(..))
import JS.Map.Primitive as P
import JS.Map.Primitive.ST as STP
-- import Foreign.Object.ST.Unsafe as STOU
import Unsafe.Coerce (unsafeCoerce)

newtype STMap r k v = STMap (STP.STMap r String (Tuple k v))

-- | Create a new, empty mutable map
new :: forall r k v. ST r (STMap r k v)
new = STMap <$> STP.new

-- | Get the value for a key in a mutable map
peek :: forall r k v. EncodeKey k => k -> STMap r k v -> ST r (Maybe v)
peek k (STMap m) = map snd <$> STP.peek (encodeKey k) m

-- | Update the value for a key in a mutable map
poke :: forall r k v. EncodeKey k => k -> v -> STMap r k v -> ST r (STMap r k v)
poke k v (STMap m) = STMap <$> STP.poke (encodeKey k) (k /\ v) m

poke_ :: forall r k v. EncodeKey k => k -> v -> STMap r k v -> ST r Unit
poke_ k v (STMap m) = STP.poke_ (encodeKey k) (k /\ v) m

-- | Remove a key and the corresponding value from a mutable map
delete :: forall r k v. EncodeKey k => k -> STMap r k v -> ST r (STMap r k v)
delete k (STMap m) = STMap <$> STP.delete (encodeKey k) m

delete_ :: forall r k v. EncodeKey k => k -> STMap r k v -> ST r Unit
delete_ k (STMap m) = STP.delete_ (encodeKey k) m

-- | Similar to alter but for mutable maps
modify :: forall r k v. EncodeKey k => k -> (Maybe v -> Maybe v) -> STMap r k v -> ST r (STMap r k v)
modify key f m = do
  mv <- f <$> peek key m
  case mv of
    Nothing -> delete key m
    Just v -> poke key v m

-- | Convert an immutable map into a mutable map
thaw :: forall r k v. Map k v -> ST r (STMap r k v)
thaw (Map m) = STMap <$> P.thawST m

run :: forall k v. (forall r. ST r (STMap r k v)) -> Map k v
run = unsafeCoerce P.runST

-- | Convert a mutable map into an immutable map
freeze :: forall r k v. STMap r k v -> ST r (Map k v)
freeze (STMap m) = Map <$> P.freezeST m

-- | Unsafely get the map out of ST without copying it
--unsafeFreeze :: forall r k v. STMap r k v -> ST r (Map k v)
--unsafeFreeze (STMap m) = Map <$> STOU.unsafeFreeze m