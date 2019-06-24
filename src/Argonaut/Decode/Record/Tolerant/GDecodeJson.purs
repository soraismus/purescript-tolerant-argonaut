module Data.Argonaut.Decode.Record.Tolerant.GDecodeJson
  ( class GDecodeJson
  , gDecodeJson
  ) where

import Prelude (class Category, class Semigroupoid, bind, identity, ($), (<<<))

import Control.Plus (class Plus, empty)
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
  (h  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  gDecodeJson
    :: RLProxy l0
    -> RLProxy l1
    -> Object Json
    -> h (p (g r0) (g r1))

instance gDecodeJson_NilNilNil
  :: ( Category p
     , Status h
     )
  => GDecodeJson p h g Nil () Nil ()
  where
  gDecodeJson _ _ _ = report identity

instance gDecodeJson_ConsNilCons_Plus
  :: ( Cons s (f v) r' r
     , D.DecodeJson (f v)
     , GDecodeJson p (Either String) g Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , Plus f
     , RInsert p g SProxy s l' r' l r
     , Semigroupoid p
     )
  => GDecodeJson p (Either String) g Nil () (Cons s (f v) l') r
  where
  gDecodeJson _ _ object = do
    doRest <- gDecodeJson nil l' object
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- D.decodeJson jsonVal
        report $ rinsert l' l s val <<< doRest
      Nothing ->
        report $ rinsert l' l s (empty :: f v) <<< doRest
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

else instance gDecodeJson_ConsNilCons_nonPlus
  :: ( Cons s v r' r
     , D.DecodeJson v
     , GDecodeJson p (Either String) g Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , RInsert p g SProxy s l' r' l r
     , Semigroupoid p
     )
  => GDecodeJson p (Either String) g Nil () (Cons s v l') r
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
     , Status h
     )
  => GDecodeJson p h g (Cons s v l') r (Cons s v l') r
  where
  gDecodeJson _ _ _ = report identity

else instance gDecodeJson_ConsConsCons_Plus
  :: ( Cons s (f v) r1' r1
     , D.DecodeJson (f v)
     , GDecodeJson p (Either String) g (Cons s1 v1 l0') r0 l1' r1'
     , IsSymbol s
     , Lacks s r0
     , Lacks s r1'
     , Plus f
     , RInsert p g SProxy s l1' r1' l1 r1
     , Semigroupoid p
     )
  => GDecodeJson p (Either String) g (Cons s1 v1 l0') r0 (Cons s (f v) l1') r1
  where
  gDecodeJson _ _ object = do
    doRest <- gDecodeJson l0 l1' object
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- D.decodeJson jsonVal
        report $ rinsert l1' l1 s val <<< doRest
      Nothing ->
        report $ rinsert l1' l1 s (empty :: f v) <<< doRest
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l0 :: RLProxy (Cons s1 v1 l0')
    l0 = RLProxy

    l1' :: RLProxy l1'
    l1' = RLProxy

    l1 :: RLProxy l1
    l1 = RLProxy

    s :: SProxy s
    s = SProxy


else instance gDecodeJson_ConsConsCons_nonPlus
  :: ( Cons s v r1' r1
     , D.DecodeJson v
     , GDecodeJson p (Either String) g (Cons s1 v1 l0') r0 l1' r1'
     , IsSymbol s
     , Lacks s r0
     , Lacks s r1'
     , RInsert p g SProxy s l1' r1' l1 r1
     , Semigroupoid p
     )
  => GDecodeJson p (Either String) g (Cons s1 v1 l0') r0 (Cons s v l1') r1
  where
  gDecodeJson _ _ object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        doRest <- gDecodeJson l0 l1' object
        report $ rinsert l1' l1 s val <<< doRest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l0 :: RLProxy (Cons s1 v1 l0')
    l0 = RLProxy

    l1' :: RLProxy l1'
    l1' = RLProxy

    l1 :: RLProxy l1
    l1 = RLProxy

    s :: SProxy s
    s = SProxy
