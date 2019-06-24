module Data.Argonaut.Decode.Record.Lazy.GDecodeJson
  ( class GDecodeJson
  , gDecodeJson
  ) where

import Prelude (class Category, class Semigroupoid, bind, identity, ($), (<<<))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson) as D
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Struct (class RInsert, rinsert)
import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class Cons, class Lacks, Cons, Nil, kind RowList)

class GDecodeJson
  (p  :: Type -> Type -> Type)
  (f  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l1 -> r1
  , l2 -> r2
  , l1 l2 -> l0
  where
  gDecodeJson
    :: RLProxy l1
    -> RLProxy l2
    -> Object Json
    -> f (p (g r1) (g r2))

instance gDecodeJson_NilNilNil
  :: ( Category p
     , Status f
     )
  => GDecodeJson p f g Nil Nil () Nil ()
  where
  gDecodeJson _ _ _ = report identity

instance gDecodeJson_ConsNilCons
  :: ( Cons s v r' r
     , D.DecodeJson v
     , GDecodeJson p (Either String) g l' Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , RInsert p g SProxy s l' r' l r
     , Semigroupoid p
     )
  => GDecodeJson p (Either String) g (Cons s v l') Nil () (Cons s v l') r
  where
  gDecodeJson _ _ object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        doRest <- gDecodeJson nil l' object
        report $ rinsert l' l s val <<< doRest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l' :: RLProxy l'
    l' = RLProxy

    l :: RLProxy l
    l = RLProxy

    nil :: RLProxy Nil
    nil = RLProxy

    s :: SProxy s
    s = SProxy

instance gDecodeJson_NilConsCons
  :: ( Category p
     , Status f
     )
  => GDecodeJson p f g Nil (Cons s v l') r (Cons s v l') r
  where
  gDecodeJson _ _ _ = report identity

else instance gDecodeJson_ConsConsCons
  :: ( Cons s v r2' r2
     , D.DecodeJson v
     , GDecodeJson p (Either String) g l0' (Cons s1 v1 l1') r1 l2' r2'
     , IsSymbol s
     , Lacks s r1
     , Lacks s r2'
     , RInsert p g SProxy s l2' r2' l2 r2
     , Semigroupoid p
     )
  => GDecodeJson
        p
        (Either String)
        g
        (Cons s v l0')
        (Cons s1 v1 l1')
        r1
        (Cons s v l2')
        r2
  where
  gDecodeJson _ _ object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        doRest <- gDecodeJson l1 l2' object
        report $ rinsert l2' l2 s val <<< doRest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l1 :: RLProxy (Cons s1 v1 l1')
    l1 = RLProxy

    l2' :: RLProxy l2'
    l2' = RLProxy

    l2 :: RLProxy l2
    l2 = RLProxy

    s :: SProxy s
    s = SProxy
