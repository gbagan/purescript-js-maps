module JS.Map.Primitive (module I) where

import JS.Map.Primitive.Internal ( Map
  , empty
  , isEmpty
  , size
  , singleton
  , insert
  , lookup
  , toUnfoldable
  , toAscUnfoldable
  , fromFoldable
  , fromFoldableWith
  , fromFoldableWithIndex
  , delete
  , pop
  , member
  , alter
  , update
  , mapWithKey
  , filterWithKey
  , filterKeys
  , filter
  , keys
  , values
  , union
  , unionWith
  , unions
  , isSubmap
  , fold
  , foldMap
  , foldM
  , foldMaybe
  , all
  , toArrayWithKey
  ) as I


