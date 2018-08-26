module ObjectsTests where

import Arbitrary()
import Test.Tasty.QuickCheck
import Data.Aeson
import Objects

prop_additionCommutative :: Point -> Point -> Bool
prop_additionCommutative a b = add a b == add b a

correctJSON :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
correctJSON a = (Just a) == (decode . encode) a

prop_correctJSON_Point :: Point -> Bool
prop_correctJSON_Point = correctJSON

prop_correctJSON_Angle :: Angle -> Bool
prop_correctJSON_Angle = correctJSON

prop_correctJSON_Shape :: Shape -> Bool
prop_correctJSON_Shape = correctJSON

prop_correctJSON_Object2d :: Object2d -> Bool
prop_correctJSON_Object2d = correctJSON

prop_correctJSON_Avatar :: Avatar -> Bool
prop_correctJSON_Avatar = correctJSON

prop_correctJSON_Obstacle :: Obstacle -> Bool
prop_correctJSON_Obstacle = correctJSON

prop_correctJSON_GameWorld :: GameWorld -> Bool
prop_correctJSON_GameWorld = correctJSON


