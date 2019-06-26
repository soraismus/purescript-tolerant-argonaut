module Data.Argonaut.Decode.Record.Lazy.Utils
  ( decodeJson
  , decodeJson'
  ) where

import Prelude (class Functor, map)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Lazy.GDecodeJson
  ( class GDecodeJson
  , gDecodeJson
  ) as D
import Data.Argonaut.Decode.Record.Utils (reportJson)
import Data.Operator.Bottom (class Bottom2)
import Foreign.Object (Object)
import Record.Builder (Builder, build)
import Type.Data.RowList (RLProxy(RLProxy))
import Type.Row (class RowToList, Nil)

decodeJson
  :: forall f l r
   . Bottom2 f String
  => D.GDecodeJson Builder f Record l Nil () l r
  => Functor f
  => RowToList r l
  => RowToList r l
  => Json
  -> f (Record r)
decodeJson json =
  map (\builder -> build builder {}) (decodeJson' json)

decodeJson'
  :: forall f g l0 l1 l2 p r1 r2
   . Bottom2 f String
  => D.GDecodeJson p f g l0 l1 r1 l2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => Json
  -> f (p (g r1) (g r2))
decodeJson' json = reportJson go json
  where
  go :: Object Json -> f (p (g r1) (g r2))
  go object =
    D.gDecodeJson
      (RLProxy :: RLProxy l1)
      (RLProxy :: RLProxy l2)
      object
