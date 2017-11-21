module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Identity (Identity(..))
import Data.Stream (stream, unstream)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  arraySpec

arraySpec :: forall r . Spec r Unit

arraySpec = describe "converting to and from arrays" do
  it "unstream <<< stream == id" do
    let testArray = [1,2,3]
    Identity testArray `shouldEqual` (unstream <<< stream $ testArray)
