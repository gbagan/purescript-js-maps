module JS.Map.Primitive.Key where

import Prelude

class (Eq v, Ord v, Show v) <= Key v

instance Key Int
instance Key String