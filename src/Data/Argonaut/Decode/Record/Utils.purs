module Data.Argonaut.Decode.Record.Utils
  ( elaborateFailure
  , getMissingFieldErrorMessage
  , msgType
  , reportJson
  , reportObject
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class GDecodeJson, gDecodeJson)
import Data.Argonaut.Decode.Record.Utils (msgType)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status (class Status, report, reportError)
import Foreign.Object (Object)
import Type.Data.RowList (RLProxy(RLProxy))
import Type.Proxy (Proxy(Proxy))
import Type.Row (class RowToList)

elaborateFailure :: forall a. String -> Either String a -> Either String a
elaborateFailure s e = lmap msg e
  where
  msg m = "Failed to decode key '" <> s <> "': " <> m

getMissingFieldErrorMessage :: String -> String
getMissingFieldErrorMessage fieldName =
  "JSON was missing expected field: " <> fieldName

msgType :: Proxy String
msgType = Proxy

notObjectErrorMessage :: String
notObjectErrorMessage = "Could not convert JSON to object"

reportJson :: forall a f. Status f String => (Object Json -> f a) -> Json -> f a
reportJson f json =
  case toObject json of
    Just object -> f object
    Nothing -> reportError notObjectErrorMessage

reportObject
  :: forall f l r
   . GDecodeJson r l
  => RowToList r l
  => Status f String
  => Object Json
  -> f (Record r)
reportObject object =
  case gDecodeJson object (RLProxy :: RLProxy l) of
    Left errorStr -> reportError errorStr
    Right record -> report msgType record
