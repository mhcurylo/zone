{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import Test.QuickCheck.Arbitrary
import Generic.Random
import Objects

instance Arbitrary Angle where
  arbitrary = genericArbitrary uniform
instance Arbitrary Shape where
  arbitrary = genericArbitrary uniform
instance Arbitrary Point where
  arbitrary = genericArbitrary uniform
instance Arbitrary Object2d where
  arbitrary = genericArbitrary uniform
instance Arbitrary Avatar where
  arbitrary = genericArbitrary uniform
instance Arbitrary Obstacle where
  arbitrary = genericArbitrary uniform
instance Arbitrary GameWorld where
  arbitrary = genericArbitrary uniform
