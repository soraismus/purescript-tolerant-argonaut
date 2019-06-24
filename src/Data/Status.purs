module Data.Status
  ( class Status
  , isError
  , report
  , reportError
  )
  where

import Prelude (const)

import Data.Array (null, singleton) as Array
import Data.Either (Either(Left, Right), isLeft)
import Data.List (List(Nil))
import Data.List (null, singleton) as List
import Data.Maybe (Maybe(Just, Nothing), isNothing)

class Status f where
  isError :: forall a. f a -> Boolean
  report :: forall a. a -> f a
  reportError :: forall a. String -> f a

instance statusArray :: Status Array where
  isError = Array.null
  report = Array.singleton
  reportError = const []

instance statusEitherString :: Status (Either String) where
  isError = isLeft
  report = Right
  reportError = Left

instance statusList :: Status List where
  isError = List.null
  report = List.singleton
  reportError = const Nil

instance statusMaybe :: Status Maybe where
  isError = isNothing
  report = Just
  reportError = const Nothing
