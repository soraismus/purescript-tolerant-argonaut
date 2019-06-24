module Test.Utils
  ( assert
  , assertEquivalence
  , check
  , withErrorMsg
  ) where

import Prelude (class Eq, class Show, show, (==), (<>), ($))

import Data.Foldable (class Foldable, foldr)
import Data.Status (class Status)
import Data.Tuple (Tuple(Tuple), uncurry)
import Test.Unit (Test)
import Test.Unit.Assert as Assert

assert :: Tuple String Boolean -> Test
assert = uncurry Assert.assert

assertEquivalence
  :: forall f a
   . Foldable f
  => Status f
  => Eq a
  => Show a
  => f a
  -> a
  -> Test
assertEquivalence result value =
  assert $ checkEquivalence result value

check
  :: forall f a
   . Foldable f
  => Status f
  => f a
  -> String
  -> (a -> Boolean)
  -> Tuple String Boolean
check result msg predicate =
  foldr
    (\val _ ->
      let
        state = predicate val
        msg' = if state then successful else msg
      in Tuple msg' state)
    (Tuple failsUnexpectedly false)
    result

checkEquivalence
  :: forall f a
   . Foldable f
  => Status f
  => Eq a
  => Show a
  => f a
  -> a
  -> Tuple String Boolean
checkEquivalence result value =
  foldr
    (\val _ ->
      let
        state = (val == value)
        msg' = if state then successful else ("Should be " <> show value)
      in Tuple msg' state)
    (Tuple failsUnexpectedly false)
    result

failsUnexpectedly :: String
failsUnexpectedly = "fails unexpectedly"

successful :: String
successful = "successful test"

withErrorMsg :: String
withErrorMsg = "doesn't meet expectations"
