module Benchmark.Main where

import Prelude
import Control.Monad.ST (foreach, run)
import Data.Array ((..))
import Data.ObjectMap as O
import Data.ObjectMap.ST as STO
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench)
import JS.Map.Primitive as P
import JS.Map.Primitive.ST as STP
import JS.Map as M
import JS.Map.ST as STM

main :: Effect Unit
main = do
  let t = (_ * 95) <$> (1..10000)
  
  log "**  (reading) **"
  log "Js.Map.Primitive (Int)"
  let m1 = P.fromFoldable $ t <#> \i -> i /\ i
  bench \_ -> t <#> \i -> P.lookup i m1 + P.lookup i m1 + P.lookup i m1
  log "Js.Map (Int)"
  let m2 = M.fromFoldable $ t <#> \i -> i /\ i
  bench \_ -> t <#> \i -> M.lookup i m2 + M.lookup i m2 + M.lookup i m2
  log "Data.ObjectMap (Int)"
  let m3 = O.fromArray $ t <#> \i -> i /\ i
  bench \_ -> t <#> \i -> O.lookup i m3 + O.lookup i m3 + O.lookup i m3

  log ""
  log "** Poke (writing) **"
  log "Js.Map.Primitive.ST (Int)"
  bench \_ -> run (do
    m <- STP.new
    foreach t \i -> do
      void $ STP.poke i i m
      void $ STP.poke (i+1) i m
      void $ STP.poke (i+2) i m
    pure 1
  )
  log "Js.Map.ST (Int)"
  bench \_ -> run (do
    m <- STM.new
    foreach t \i -> do
      void $ STM.poke i i m
      void $ STM.poke (i+1) i m
      void $ STM.poke (i+2) i m
    pure 1
  )
  log "Data.ObjectMap (Int)"
  bench \_ -> run (do
    m <- STO.new
    foreach t \i -> do
      void $ STO.poke i i m
      void $ STO.poke (i+1) i m
      void $ STO.poke (i+2) i m
    pure 1
  )
