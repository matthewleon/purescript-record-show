module Data.Record.ShowRecord (
  ShowRecord(..)
, class ShowRowList
, showRowList
) where

import Prelude

import Data.List.Lazy as L
import Data.Newtype (class Newtype)
import Data.Record (get, delete)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Prelude (class RowToList, class RowLacks)
import Type.Row (kind RowList, Nil, Cons, RLProxy(..))

newtype ShowRecord r = ShowRecord r
derive instance newtypeShowRecord :: Newtype (ShowRecord r) _

class ShowRowList (list :: RowList) (row :: # Type) | list -> row where
  showRowList :: RLProxy list -> Record row -> L.List String

instance showRowListNil :: ShowRowList Nil () where
  showRowList _ _ = L.nil

instance showRowListCons :: 
  ( Show a
  , ShowRowList listRest rowRest
  , RowCons  key a rowRest rowFull
  , RowLacks key rowRest
  , RowToList rowFull (Cons key a listRest)
  , IsSymbol key
  ) => ShowRowList (Cons key a listRest) rowFull where
  showRowList _ rec = (reflectSymbol key <> ": " <> show val) `L.cons` rest
    where
    key = SProxy :: SProxy key
    val = get key rec
    rest = showRowList (RLProxy :: RLProxy listRest) (delete key rec)

instance showShowRecord ::
  ( RowToList row list
  , ShowRowList list row
  ) => Show (ShowRecord (Record row)) where
  show (ShowRecord rec) = "{ " <> L.intercalate ", " rowListStrs <> " }"
    where
    rowListStrs = showRowList (RLProxy ::RLProxy list) rec
