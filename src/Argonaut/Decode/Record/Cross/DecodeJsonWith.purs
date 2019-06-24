module Data.Argonaut.Decode.Record.Cross.Class
  ( class DecodeJsonWith
  , decodeJsonWith
  ) where

import Prelude
  ( class Bind
  , class Category
  , class Semigroupoid
  , bind
  , identity
  , ($)
  , (<<<)
  )

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.Struct (class RGet, class RInsert, rget, rinsert)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
import Type.Row (class Cons, class Lacks, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonWith
  (p  :: Type -> Type -> Type)
  (f  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  (r3 :: # Type)
  a
  | l0 -> r0 a
  , l2 -> r2
  , l0 l2 -> r3
  where
  decodeJsonWith
    :: RLProxy l0
    -> RLProxy l2
    -> g r0
    -> Object Json
    -> a
    -> f (p (g r2) (g r3))

instance decodeJsonWithNil
  :: ( Category p
     , Status f
     )
  => DecodeJsonWith p f g Nil () l r r a
  where
  decodeJsonWith _ _ _ _ _ = report identity

instance decodeJsonWithCons
  :: ( Bind f
     , Cons s fn r0' r0
     , Cons s v r3' r3
     , DecodeJsonWith p f g l0' r0' l2 r2 r3' a
     , IsSymbol s
     , Status f
     , Lacks s r3'
     , RGet g SProxy s l0 r0
     , RInsert p g SProxy s l3' r3' l3 r3
     , Semigroupoid p
     , TypeEquals fn (Json -> a -> f v)
     )
  => DecodeJsonWith p f g (Cons s fn l0') r0 l2 r2 r3 a
  where
  decodeJsonWith _ _ decoderRecord object x = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        doRest <- decodeJsonWith l0' l2 decoderRecord' object x
        report $ rinsert l3' l3 s val <<< doRest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    decoder :: Json -> a -> f v
    decoder = to $ rget l0 s decoderRecord

    -- To prevent unnecessary creation of intermediate decoder records,
    -- coercion is used rather than calling `Record.delete s`
    -- to induce the next expected type.
    decoderRecord' :: g r0'
    decoderRecord' = unsafeCoerce decoderRecord

    fieldName :: String
    fieldName = reflectSymbol s

    l0 :: RLProxy l0
    l0 = RLProxy

    l0' :: RLProxy l0'
    l0' = RLProxy

    l2 :: RLProxy l2
    l2 = RLProxy

    l3 :: RLProxy l3
    l3 = RLProxy

    l3' :: RLProxy l3'
    l3' = RLProxy

    s :: SProxy s
    s = SProxy
