module JS.Map (module I) 
    where

import JS.Map.Internal
  ( Map
  , empty
  , isEmpty
  , size
  , fromFoldable
  , toArray
  , member
  , lookup
  , insert
  , alter
  , update
  , delete
  ) as I

import JS.Map.EncodeKey (class EncodeKey, encodeKey) as I