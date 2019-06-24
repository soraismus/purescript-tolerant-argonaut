module Data.Status
  ( class Status
  , report
  , reportError
  , summarize
  )
  where

import Prelude

import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Just, Nothing), maybe)

class Status f where
  report :: forall a. a -> f a
  reportError :: forall a. String -> f a
  summarize :: forall a b. a -> (b -> a) -> f b -> a

instance statusEitherString :: Status (Either String) where
  report = Right
  reportError = Left
  summarize x = either (const x)

instance statusMaybe :: Status Maybe where
  report = Just
  reportError = const Nothing
  summarize = maybe
