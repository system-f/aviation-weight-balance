{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.WB.Arm.Arm(
  Arm(..)
, staticArm
, rangeArm
, HasArm(..)
, HasArms(..)
, SetArm(..)
, HasArm0(..)
) where

import Control.Category((.))
import Control.Lens(Lens', Traversal', Setter', lens, makeClassy)
import Data.Aviation.WB.Arm.ArmRangeLower(HasArmRangeLowers(armRangeLowers), SetArmRangeLower(setArmRangeLower))
import Data.Aviation.WB.Arm.ArmRange(ArmRange, HasArmRanges(armRanges), SetArmRange(setArmRange), HasArmRange0(armRange0))
import Data.Aviation.WB.Arm.ArmStatic(ArmStatic, HasArmStatic(armStatic), HasArmStatics(armStatics), SetArmStatic(setArmStatic))
import Data.Aviation.WB.Arm.ArmRangeUpper(HasArmRangeUppers(armRangeUppers), SetArmRangeUpper(setArmRangeUpper))
import Data.Functor((<$>))
import Data.Traversable(traverse)
import Data.Eq(Eq)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Prelude(Show)
    
data Arm =
  Arm
    ArmStatic
    (Maybe ArmRange)
  deriving (Eq, Ord, Show)

makeClassy ''Arm

staticArm ::
  ArmStatic
  -> Arm
staticArm x =
  Arm x Nothing

rangeArm ::
  ArmStatic
  -> ArmRange
  -> Arm
rangeArm x =
  Arm x . Just

class HasArms a where
  arms ::
    Traversal'
      a
      Arm

instance HasArms Arm where
  arms =
    arm

class SetArm a where
  setArm ::
    Setter'
      a
      Arm

instance SetArm Arm where
  setArm =
    arm

instance HasArmStatic Arm where
  armStatic =
    lens
      (\(Arm s _) -> s)
      (\(Arm _ r) s -> Arm s r)

instance HasArmStatics Arm where
  armStatics =
    armStatic
    
instance HasArmRanges Arm where
  armRanges f (Arm s r) =
    Arm s <$> traverse f r

instance SetArmRange Arm where
  setArmRange =
    armRanges

instance SetArmStatic Arm where
  setArmStatic =
    armStatic . setArmStatic

instance HasArmRangeLowers Arm where
  armRangeLowers =
    armRanges . armRangeLowers

instance HasArmRangeUppers Arm where
  armRangeUppers =
    armRanges . armRangeUppers

instance SetArmRangeLower Arm where
  setArmRangeLower =
    setArmRange . setArmRangeLower

instance SetArmRangeUpper Arm where
  setArmRangeUpper =
    setArmRange . setArmRangeUpper

class HasArm0 a where
  arm0 ::
    Lens'
      a
      (Maybe Arm)

instance HasArmRange0 Arm where
  armRange0 =
    lens
      (\(Arm _ r) -> r)
      (\(Arm s _) r -> Arm s r)
