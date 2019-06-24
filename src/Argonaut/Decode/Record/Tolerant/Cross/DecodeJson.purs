module Data.Argonaut.Decode.Record.Tolerant.Cross.Utils
  ( decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Tolerant.GDecodeJson
  ( class GDecodeJson
  , gDecodeJson
  )
import Data.Argonaut.Decode.Record.Cross.Class
  ( class DecodeJsonWith
  , decodeJsonWith
  ) as D
import Data.Argonaut.Decode.Record.Utils (reportJson)
import Data.Status (class Status, report)
import Foreign.Object (Object)
import Record.Builder (Builder, build)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList, Nil)

decodeJsonWith
  :: forall f l0 l2 r0 r2 r3
   . Bind f
  => D.DecodeJsonWith Builder f Record l0 r0 l2 r2 r3 (Record r2)
  => GDecodeJson Builder f Record Nil () l2 r2
  => RowToList r0 l0
  => RowToList r2 l2
  => Status f
  => Record r0
  -> Json
  -> f (Record r3)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r3)
  go object = do
    addFields2 <-
      gDecodeJson
        (RLProxy :: RLProxy Nil)
        (RLProxy :: RLProxy l2)
        object
    let record2 = build addFields2 {}
    addFields0 <-
      D.decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l2)
        decoderRecord
        object
        record2
    report $ build addFields0 record2
