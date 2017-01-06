{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.WB.Arm.ArmRange(
  ArmRange(..)
, HasArmRange(..)
, HasArmRanges(..)
, SetArmRange(..)
, HasArmRange0(..)
, (.->.)
) where

import Control.Lens(Lens', Traversal', Setter', lens, makeClassy)
import Data.Aviation.WB.Arm.ArmRangeLower(ArmRangeLower, HasArmRangeLower(armRangeLower), HasArmRangeLowers(armRangeLowers), SetArmRangeLower(setArmRangeLower))
import Data.Aviation.WB.Arm.ArmRangeUpper(ArmRangeUpper, HasArmRangeUpper(armRangeUpper), HasArmRangeUppers(armRangeUppers), SetArmRangeUpper(setArmRangeUpper))
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Prelude(Show)

data ArmRange =
  ArmRange
    ArmRangeLower
    ArmRangeUpper
  deriving (Eq, Ord, Show)

makeClassy ''ArmRange

(.->.) ::
  ArmRangeLower
  -> ArmRangeUpper
  -> ArmRange
(.->.) =
  ArmRange

infixl 2 .->. 

class HasArmRanges a where
  armRanges ::
    Traversal'
      a
      ArmRange

instance HasArmRanges ArmRange where
  armRanges =
    armRange

class SetArmRange a where
  setArmRange ::
    Setter'
      a
      ArmRange

instance SetArmRange ArmRange where
  setArmRange =
    armRange

instance HasArmRangeLower ArmRange where
  armRangeLower =
    lens
      (\(ArmRange lower _) -> lower)
      (\(ArmRange _ upper) lower -> ArmRange lower upper)

instance HasArmRangeUpper ArmRange where
  armRangeUpper =
    lens
      (\(ArmRange _ upper) -> upper)
      (\(ArmRange lower _) upper -> ArmRange lower upper)

instance HasArmRangeLowers ArmRange where
  armRangeLowers =
    armRangeLower
    
instance HasArmRangeUppers ArmRange where
  armRangeUppers =
    armRangeUpper

instance SetArmRangeLower ArmRange where
  setArmRangeLower =
    armRangeLower
    
instance SetArmRangeUpper ArmRange where
  setArmRangeUpper =
    armRangeUpper

class HasArmRange0 a where
  armRange0 ::
    Lens'
      a
      (Maybe ArmRange)
