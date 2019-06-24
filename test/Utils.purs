module Test.Utils
  ( assert
  , assertEquivalence
  , check
  , withErrorMsg
  ) where

import Prelude (class Eq, class Show, show, (==), (<>), ($))

import Data.Status (class Status, summarize)
import Data.Tuple (Tuple(Tuple), uncurry)
import Test.Unit (Test)
import Test.Unit.Assert as Assert

assert :: Tuple String Boolean -> Test
assert = uncurry Assert.assert

assertEquivalence
  :: forall f a
   . Status f
  => Eq a
  => Show a
  => f a
  -> a
  -> Test
assertEquivalence result value =
  assert $ checkEquivalence result value

check
  :: forall f a
   . Status f
  => f a
  -> String
  -> (a -> Boolean)
  -> Tuple String Boolean
check result msg predicate =
  summarize
    (Tuple failsUnexpectedly false)
    (\val ->
      let
        state = predicate val
        msg' = if state then successful else msg
      in Tuple msg' state)
    result

checkEquivalence
  :: forall f a
   . Status f
  => Eq a
  => Show a
  => f a
  -> a
  -> Tuple String Boolean
checkEquivalence result value =
  summarize
    (Tuple failsUnexpectedly false)
    (\val ->
      let
        state = (val == value)
        msg' = if state then successful else ("Should be " <> show value)
      in Tuple msg' state)
    result

failsUnexpectedly :: String
failsUnexpectedly = "fails unexpectedly"

successful :: String
successful = "successful test"

withErrorMsg :: String
withErrorMsg = "doesn't meet expectations"
