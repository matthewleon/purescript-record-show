module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Record.ShowRecord (showRecord)
import Test.Assert (ASSERT, assert)

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = assert $ showRecord { a: 1, b: 2, c: "foo", d: "bar" }
                == "{ a: 1, b: 2, c: \"foo\", d: \"bar\" }"
