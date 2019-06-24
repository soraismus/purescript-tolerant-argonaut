module Data.Argonaut.Decode.Record.Override.DecodeJsonWith
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
import Type.Row
  ( class Cons
  , class Lacks
  , Cons
  , Nil
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonWith
  (p  :: Type -> Type -> Type)
  (f  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  decodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> g r0
    -> Object Json
    -> f (p (g r1) (g r2))

instance decodeJsonWithNil
  :: ( Category p
     , Status f
     )
  => DecodeJsonWith p f g Nil () l r r
  where
  decodeJsonWith _ _ _ _ = report identity

instance decodeJsonWithCons
  :: ( Bind f
     , Cons s fn r0' r0
     , Cons s v r2' r2
     , DecodeJsonWith p f g l0' r0' l1 r1 r2'
     , IsSymbol s
     , Status f
     , Lacks s r2'
     , RGet g SProxy s l0 r0
     , RInsert p g SProxy s l2' r2' l2 r2
     , Semigroupoid p
     , TypeEquals fn (Json -> f v)
     )
  => DecodeJsonWith p f g (Cons s fn l0') r0 l1 r1 r2
  where
  decodeJsonWith _ _ decoderRecord object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        doRest <- decodeJsonWith l0' l1 decoderRecord' object
        report $ rinsert l2' l2 s val <<< doRest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    decoder :: Json -> f v
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

    l1 :: RLProxy l1
    l1 = RLProxy

    l2 :: RLProxy l2
    l2 = RLProxy

    l2' :: RLProxy l2'
    l2' = RLProxy

    s :: SProxy s
    s = SProxy
