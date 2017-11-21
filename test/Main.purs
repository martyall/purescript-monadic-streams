module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Identity (Identity(..))
import Data.Array ((:))
import Data.Stream (foldlStream, foldrStream, stream, unstream)
import Data.String (fromCharArray)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  arraySpec
  foldSpec

arraySpec :: forall r . Spec r Unit
arraySpec = describe "converting to and from arrays" do
  it "unstream <<< stream == id" do
    let testArray = [1,2,3]
    Identity testArray `shouldEqual` (unstream <<< stream $ testArray)

foldSpec :: forall r . Spec r Unit
foldSpec = describe "folding over a stream" do
  it "can sum with foldl" do
    let as = stream [1,2,3]
    foldlStream (+) 0 as `shouldEqual` Identity 6
  it "can build a string with foldr" do
    let s = "hello"
    Identity s `shouldEqual` (fromCharArray <$> foldrStream (:) [] (stream ['h','e','l','l','o']))
