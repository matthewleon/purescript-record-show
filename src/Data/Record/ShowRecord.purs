module Data.Record.ShowRecord (
  class ShowRowList
, showRowList
, showRecord
) where

import Prelude

import Data.List.Lazy as L
import Data.Record (get, delete)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Prelude (class RowToList, class RowLacks)
import Type.Row (kind RowList, Nil, Cons, RLProxy(..))

class ShowRowList (list :: RowList) (row :: # Type) | list -> row where
  showRowList :: RLProxy list -> Record row -> L.List String

instance showRowListNil :: ShowRowList Nil () where
  showRowList _ _ = L.nil

instance showRowListConsRecord ::
  ( RowToList subrow sublist
  , ShowRowList sublist subrow
  , ShowRowList listRest rowRest
  , RowCons  key (Record subrow) rowRest rowFull
  , RowLacks key rowRest
  , RowToList rowFull (Cons key (Record subrow) listRest)
  , IsSymbol key
  ) => ShowRowList (Cons key (Record subrow) listRest) rowFull where
  showRowList _ rec =
    (reflectSymbol key <> ": " <> showRecord val) `L.cons` rest
    where
    key = SProxy :: SProxy key
    val = get key rec
    rest = showRowList (RLProxy :: RLProxy listRest) (delete key rec)

instance showRowListConsShow ::
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


showRecord
  :: forall row list
   . RowToList row list
  => ShowRowList list row
  => Record row
  -> String
showRecord rec = "{ " <> L.intercalate ", " rowListStrs <> " }"
  where
  rowListStrs = showRowList (RLProxy ::RLProxy list) rec
