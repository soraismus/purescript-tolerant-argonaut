module Data.Status
  ( class Status
  , report
  , reportError
  )
  where

import Prelude (const)

import Data.Array (singleton) as Array
import Data.Either (Either(Left, Right))
import Data.List (List(Nil))
import Data.List (singleton) as List
import Data.Maybe (Maybe(Just, Nothing))

class Status f where
  report :: forall a. a -> f a
  reportError :: forall a. String -> f a

instance statusArray :: Status Array where
  report = Array.singleton
  reportError = const []

instance statusEitherString :: Status (Either String) where
  report = Right
  reportError = Left

instance statusList :: Status List where
  report = List.singleton
  reportError = const Nil

instance statusMaybe :: Status Maybe where
  report = Just
  reportError = const Nothing
