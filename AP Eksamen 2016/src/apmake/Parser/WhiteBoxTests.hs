module Parser.WhiteBoxTests where

-- import Ast
-- import Parser.Impl

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
main = defaultMain $ testGroup "White-box parser tests"
  [ unitTests, qcTests, goldenTests ]
