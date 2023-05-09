module JS.Map (module I) 
    where

import JS.Map.Internal
  ( Map
  , empty
  , isEmpty
  , singleton
  , size
  , fromFoldable
  , toArray
  , member
  , lookup
  , insert
  , alter
  , update
  , delete
  , filter
  , filterKeys
  , filterWithKey
  ) as I

import JS.Map.EncodeKey (class EncodeKey, encodeKey) as I