module Interp.WhiteBoxTests where

-- import Ast
-- import Interp.Impl

import Test.Tasty
-- import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck
-- import Test.Tasty.Golden

unitTests :: TestTree
unitTests = testGroup "Unit tests" []

qcTests :: TestTree
qcTests = testGroup "QuickCheck tests" []

goldenTests :: TestTree
goldenTests = testGroup "Golden tests" []

main :: IO ()
main = defaultMain $ testGroup "White-box interpreter tests"
  [ unitTests, qcTests, goldenTests ]
