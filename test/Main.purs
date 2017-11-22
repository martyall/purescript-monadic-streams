module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array ((:))
import Data.Identity (Identity(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Stream (foldMap, foldl, foldr, stream, unstream, enumStream, mapM) as Stream
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  arraySpec
  foldSpec
  enumSpec
  mapMSpec

arraySpec :: forall r . Spec r Unit
arraySpec = describe "converting to and from arrays" do
  it "unstream <<< stream == id" do
    let testArray = [1,2,3]
    Identity testArray `shouldEqual` (Stream.unstream <<< Stream.stream $ testArray)

foldSpec :: forall r . Spec r Unit
foldSpec = describe "folding over a stream" do
  it "can sum with foldl" do
    let as = Stream.stream [1,2,3]
    Stream.foldl (+) 0 as `shouldEqual` Identity 6
  it "can build a string with foldr" do
    let s = "hello"
    Identity s `shouldEqual` (fromCharArray <$> Stream.foldr (:) [] (Stream.stream ['h','e','l','l','o']))
  it "can sum with fold" do
    let as = map Additive $ Stream.stream [1,2,3]
        folded = unwrap <$> Stream.foldMap (\a -> negate <$> a) as
    folded `shouldEqual` Identity (- 6)

enumSpec :: forall r . Spec r Unit
enumSpec = describe "generating an enum stream" do
  it "can generate an enum stream" do
    let as = Stream.enumStream $ Tuple 1 3
    Stream.unstream as `shouldEqual` Identity [1,2,3]

mapMSpec ::  forall r . Spec r Unit
mapMSpec = describe "test mapM" do
  it "can mapM over a stream" do
    let f = \a -> Identity (a + 1)
        as = Stream.stream [1,2,3]
        bs = Stream.mapM f as
    Stream.unstream bs `shouldEqual` Identity [2,3,4]
