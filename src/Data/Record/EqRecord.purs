module Data.Record.EqRecord (
  class EqRowList
, eqRowList
, eqRecord
) where

import Prelude

import Data.Record (get)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Type.Prelude (class RowToList)
import Type.Row (kind RowList, Nil, Cons, RLProxy(..))

-- | Eq for subrecord given by a row list
class EqRowList (list :: RowList) (row :: # Type) where
  eqRowList :: RLProxy list -> Record row → Record row -> Boolean

instance _a_eqRowListNil :: EqRowList Nil a where
  eqRowList _ _ _ = true

instance _b_eqRowListConsRecord ::
  ( EqRowList rl r
  , RowToList r rl
  , EqRowList listRest row
  , IsSymbol key
  , RowCons key (Record r) t row) => EqRowList (Cons key (Record r) listRest) row where
  eqRowList _ rec1 rec2 =
    eqRowList (RLProxy ∷ RLProxy rl) s1 s2
    && eqRowList (RLProxy ∷ RLProxy listRest) rec1 rec2
   where
    s1 = get (SProxy ∷ SProxy key) rec1
    s2 = get (SProxy ∷ SProxy key) rec2

instance _c_eqRowListCons ::
  ( Eq a
  , EqRowList listRest row
  , IsSymbol key
  , RowCons key a r row) => EqRowList (Cons key a listRest) row where
  eqRowList _ rec1 rec2 =
    get (SProxy ∷ SProxy key) rec1 == get (SProxy ∷ SProxy key) rec2
    && eqRowList (RLProxy ∷ RLProxy listRest) rec1 rec2

eqRecord
  :: forall row list
   . RowToList row list
  => EqRowList list row
  => Record row
  → Record row
  -> Boolean
eqRecord rec1 rec2 = eqRowList (RLProxy ∷ RLProxy list) rec1 rec2
