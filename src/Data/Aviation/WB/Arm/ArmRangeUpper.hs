{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.WB.Arm.ArmRangeUpper(
  ArmRangeUpper
, HasArmRangeUpper(..)
, HasArmRangeUppers(..)
, SetArmRangeUpper(..)
, HasArmRangeUpper0(..)
) where

import Control.Category((.))
import Control.Lens(Lens', Traversal', Setter', makeClassy, iso)
import Data.Aviation.Units(Inches(inches), Centimetres(centimetres), Thouinches(thouinches))
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord)
import Data.Ratio((%))
import Data.Semigroup(Semigroup((<>)))
import Numeric.Lens(dividing, multiplying)
import Prelude(Show, Rational, (+))

newtype ArmRangeUpper =
  ArmRangeUpper
    Rational
  deriving (Eq, Ord, Show)

makeClassy ''ArmRangeUpper

class HasArmRangeUppers a where
  armRangeUppers ::
    Traversal'
      a
      ArmRangeUpper

instance HasArmRangeUppers ArmRangeUpper where
  armRangeUppers =
    armRangeUpper

class SetArmRangeUpper a where
  setArmRangeUpper ::
    Setter'
      a
      ArmRangeUpper

instance SetArmRangeUpper ArmRangeUpper where
  setArmRangeUpper =
    armRangeUpper

class HasArmRangeUpper0 a where
  armRangeUpper0 ::
    Lens'
      a
      (Maybe ArmRangeUpper)

instance Inches ArmRangeUpper where
  inches =
    iso
      ArmRangeUpper
      (\(ArmRangeUpper x) -> x)

instance Thouinches ArmRangeUpper where
  thouinches =
    multiplying 1000 . inches

instance Centimetres ArmRangeUpper where
  centimetres =
    dividing (254 % 100) . inches
    
instance Semigroup ArmRangeUpper where
  (<>) =
    mappend

instance Monoid ArmRangeUpper where
  mempty =
    ArmRangeUpper 0
  ArmRangeUpper w1 `mappend` ArmRangeUpper w2 =
    ArmRangeUpper (w1 + w2)
