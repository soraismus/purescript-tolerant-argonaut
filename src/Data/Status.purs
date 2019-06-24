module Data.Status
  ( class Status
  , isError
  , report
  , reportError
  , summarize
  )
  where

import Prelude

import Data.Either (Either(Left, Right), either, isLeft)
import Data.Maybe (Maybe(Just, Nothing), isNothing, maybe)

class Status f where
  isError :: forall a. f a -> Boolean
  report :: forall a. a -> f a
  reportError :: forall a. String -> f a
  summarize :: forall a b. a -> (b -> a) -> f b -> a

instance statusEitherString :: Status (Either String) where
  isError = isLeft
  report = Right
  reportError = Left
  summarize x = either (const x)

instance statusMaybe :: Status Maybe where
  isError = isNothing
  report = Just
  reportError = const Nothing
  summarize = maybe
