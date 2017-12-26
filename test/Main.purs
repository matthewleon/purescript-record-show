module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Record.ShowRecord (showRecord)
import Data.Record.EqRecord (eqRecord)
import Test.Assert (ASSERT, assert)

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
  assert $ showRecord { a: 1, b: 2, c: "foo", d: "bar" }
           == "{ a: 1, b: 2, c: \"foo\", d: \"bar\" }"

  -- this will produce an Overlapping Instance warning, but work.
  assert $ showRecord { a: 1, b: { c: "foo" } }
           == "{ a: 1, b: { c: \"foo\" } }"

  assert $ eqRecord {a: 1, b: "b"} {a: 1, b: "b"}

  assert $ eqRecord { a: 1, b: { c : "foo" }} {a: 1, b: { c: "foo" }}
