module Data.Argonaut.Decode.Record.Tolerant.DecodeJson
  ( class DecodeJson
  , decodeJson
  ) where

import Prelude (bind, ($))

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson) as D
import Data.Argonaut.Decode.Record.Tolerant.GDecodeJson
  ( class GDecodeJson
  , gDecodeJson
  ) as G
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status (report, reportError)
import Record.Builder (Builder, build)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList, Nil, kind RowList)

class DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeRecord
  :: ( G.GDecodeJson Builder (Either String) Record Nil () l r
     , RowToList r l
     )
  => DecodeJson (Record r)
  where
  decodeJson json =
    case toObject json of
      Just object -> do
        builder <-
          G.gDecodeJson
            (RLProxy :: RLProxy Nil)
            (RLProxy :: RLProxy l)
            object
        report $ build builder {}
      Nothing ->
        reportError "Could not convert JSON to object"

else instance decodeDecodeJson :: D.DecodeJson a => DecodeJson a where
  decodeJson = D.decodeJson
