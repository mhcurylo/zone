{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import Test.QuickCheck.Arbitrary
import Generic.Random
import Objects
import Data.Hashable
import Data.HashMap.Strict

instance Arbitrary Angle where
  arbitrary = genericArbitrary uniform
instance Arbitrary Shape where 
  arbitrary = genericArbitrary uniform
instance Arbitrary Point where
  arbitrary = genericArbitrary uniform
instance (Arbitrary v, Arbitrary k, Hashable k, Ord k) => Arbitrary (HashMap k v) where
  arbitrary = do
   (kv :: [(k, v)]) <- arbitrary
   return $ fromList kv 
instance Arbitrary Object2d where
  arbitrary = genericArbitrary uniform
instance Arbitrary Avatar where
  arbitrary = genericArbitrary uniform
instance Arbitrary Obstacle where
  arbitrary = genericArbitrary uniform
instance Arbitrary GameWorld where
  arbitrary = genericArbitrary uniform
