module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.Cross (suites) as Cross
import Test.Suites.Lazy (suites) as Lazy
import Test.Suites.Override (suites) as Override
import Test.Suites.Tolerant (suites) as Tolerant
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Cross.suites
  Lazy.suites
  Override.suites
  Tolerant.suites
