module Data.Argonaut.Decode.Record.Override.Utils
  ( decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class GDecodeJson)
import Data.Argonaut.Decode.Record.Override.DecodeJsonWith
  ( class DecodeJsonWith
  , decodeJsonWith
  ) as D
import Data.Argonaut.Decode.Record.Utils (reportJson, reportObject)
import Data.Status (class Status, report)
import Foreign.Object (Object)
import Record.Builder (Builder, build)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList)

decodeJsonWith
  :: forall f l0 l1 r0 r1 r2
   . Bind f
  => D.DecodeJsonWith Builder f Record l0 r0 l1 r1 r2
  => GDecodeJson r1 l1
  => RowToList r0 l0
  => RowToList r1 l1
  => Status f
  => Record r0
  -> Json
  -> f (Record r2)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    (record1 :: Record r1) <- reportObject object
    (addFields0 :: Builder (Record r1) (Record r2)) <- D.decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord
        object
    report $ build addFields0 record1
