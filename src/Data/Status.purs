module Data.Status
  ( class Status
  , report
  , reportError
  , summarize
  )
  where

import Prelude (const, (<<<))

import Data.Array (head, singleton) as Array
import Data.Either (Either(Left, Right), either)
import Data.List (List(Nil))
import Data.List (head, singleton) as List
import Data.Maybe (Maybe(Just, Nothing), maybe)

class Status f where
  report :: forall a. a -> f a
  reportError :: forall a. String -> f a
  summarize :: forall a b. a -> (b -> a) -> f b -> a

instance statusArray :: Status Array where
  report = Array.singleton
  reportError = const []
  summarize x f = maybe x f <<< Array.head

instance statusEitherString :: Status (Either String) where
  report = Right
  reportError = Left
  summarize x = either (const x)

instance statusList :: Status List where
  report = List.singleton
  reportError = const Nil
  summarize x f = maybe x f <<< List.head

instance statusMaybe :: Status Maybe where
  report = Just
  reportError = const Nothing
  summarize = maybe
