module Test.Utils
  ( assert
  , assertEquivalence
  , check
  , check'
  , checkError
  , capriciousFailure
  , doesntFail
  , doesntMeetExpectations
  , fails
  , failsUnexpectedly
  , labels
  , noTolerance
  , notVal0
  , notVal1
  , notVal2
  , notVal3
  , notVal4
  , notVal5
  , successful
  , withErrorMsg
  ) where

import Prelude (class Eq, class Show, show, (==), (<>), ($))

import Data.Either (Either)
import Data.Status (class Status, isError, summarize)
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

capriciousFailure :: String
capriciousFailure = "capricious failure"

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

check'
  :: forall f a
   . Status f
  => f a
  -> (a -> Boolean)
  -> Boolean
  -> String
  -> Tuple String Boolean
check' result predicate _ msg = check result msg predicate

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

checkError
  :: forall a
   . Either String a
  -> String
  -> (a -> Boolean)
  -> Tuple String Boolean
checkError = check

doesntFail :: String
doesntFail = "is decoded despite expectation of failure"

doesntMeetExpectations :: String
doesntMeetExpectations = "doesn't meet expectations"

fails :: forall f a . Status f => f a -> Tuple String Boolean
fails result =
  if isError result
    then Tuple successful true
    else Tuple doesntFail false

failsUnexpectedly :: String
failsUnexpectedly = "fails unexpectedly"

labels :: String
labels = "Tolerant labels: "

notVal0 :: String
notVal0 = "isn't equal to { a0: 0, a1: 1 }"

notVal1 :: String
notVal1 = "isn't equal to { a0: 0, a1: 1, a2: Just 2 }"

notVal2 :: String
notVal2 = "isn't equal to { a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }"

notVal3 :: String
notVal3 = "isn't equal to { a0: 0, a1: 1 }"

notVal4 :: String
notVal4 = "isn't equal to { a0: 0, a1: 1, a2: [2] }"

notVal5 :: String
notVal5 = "isn't equal to { a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }"

noTolerance :: String
noTolerance = "No Tolerance for Absent Fields"

successful :: String
successful = "successful test"

withErrorMsg :: String
withErrorMsg = doesntMeetExpectations
