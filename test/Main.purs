module Test.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import JS.Map.Primitive as P
import JS.Map.Primitive.ST as STP
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

sample :: P.Map Int String
sample = P.empty
  # P.insert 2 "2"
  # P.insert 3 "3"
  # P.insert 1 "1"

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "insertion" do
    it "keeps order" do
      P.empty
        # P.insert "b" 2
        # P.insert "c" 3
        # P.insert "a" 1
        # P.toUnfoldable
        # shouldEqual
            [ "b" /\ 2
            , "c" /\ 3
            , "a" /\ 1
            ]
  describe "bulk insertion with numbers" do
    it "keeps order" do
      P.fromFoldable
        [ 2 /\ 2
        , 3 /\ 3
        , 1 /\ 1
        ]
        # P.toUnfoldable
        # shouldEqual
            [ 2 /\ 2
            , 3 /\ 3
            , 1 /\ 1
            ]

  describe "build an object map via ST" do
    it "keeps order" do
      let m' = P.runST (do
            m <- STP.new
            _ <- STP.poke 2 3 m
            _ <- STP.poke 3 3 m
            _ <- STP.poke 1 1 m
            pure m
      )
      P.toUnfoldable m' # shouldEqual
            [ 2 /\ 3
            , 3 /\ 3
            , 1 /\ 1
            ]