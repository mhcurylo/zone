module ObjectsTests where

import Test.Tasty.QuickCheck
import Objects

prop_additionCommutative :: Point -> Point -> Bool
prop_additionCommutative a b = a + b == b + a
