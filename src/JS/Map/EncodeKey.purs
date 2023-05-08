module JS.Map.EncodeKey where

import Prelude
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

class EncodeKey a where
  encodeKey :: a -> String

instance EncodeKey Boolean where
  encodeKey = show

instance EncodeKey Int where
  encodeKey = show

instance EncodeKey String where
  encodeKey = show

instance (EncodeKey a, EncodeKey b) => EncodeKey (Tuple a b) where
  encodeKey (Tuple a b) = "(" <> encodeKey a <> "," <> encodeKey b <> ")"

instance EncodeKey a => EncodeKey (Array a) where
  encodeKey l = "[" <> joinWith "," (map encodeKey l) <> "]"