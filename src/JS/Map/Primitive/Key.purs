module JS.Map.Primitive.Key where

import Prelude
import JS.BigInt (BigInt)

class (Eq v, Ord v, Show v) <= Key v

instance Key Int
instance Key String
instance Key BigInt