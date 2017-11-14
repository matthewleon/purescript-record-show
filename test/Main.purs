module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Record.ShowRecord (ShowRecord(..))
import Test.Assert (ASSERT, assert)

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = assert $ show (ShowRecord { a: 1, b: 2, c: "foo", d: "bar" })
                == "{ a: 1, b: 2, c: \"foo\", d: \"bar\" }"
