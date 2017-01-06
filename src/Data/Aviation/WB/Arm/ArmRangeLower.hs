{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.WB.Arm.ArmRangeLower(
  ArmRangeLower
, HasArmRangeLower(..)
, HasArmRangeLowers(..)
, SetArmRangeLower(..)
, HasArmRangeLower0(..)
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

newtype ArmRangeLower =
  ArmRangeLower
    Rational
  deriving (Eq, Ord, Show)

makeClassy ''ArmRangeLower

class HasArmRangeLowers a where
  armRangeLowers ::
    Traversal'
      a
      ArmRangeLower

instance HasArmRangeLowers ArmRangeLower where
  armRangeLowers =
    armRangeLower

class SetArmRangeLower a where
  setArmRangeLower ::
    Setter'
      a
      ArmRangeLower

instance SetArmRangeLower ArmRangeLower where
  setArmRangeLower =
    armRangeLower

class HasArmRangeLower0 a where
  armRangeLower0 ::
    Lens'
      a
      (Maybe ArmRangeLower)

instance Inches ArmRangeLower where
  inches =
    iso
      ArmRangeLower
      (\(ArmRangeLower x) -> x)

instance Thouinches ArmRangeLower where
  thouinches =
    multiplying 1000 . inches

instance Centimetres ArmRangeLower where
  centimetres =
    dividing (254 % 100) . inches

instance Semigroup ArmRangeLower where
  (<>) =
    mappend

instance Monoid ArmRangeLower where
  mempty =
    ArmRangeLower 0
  ArmRangeLower w1 `mappend` ArmRangeLower w2 =
    ArmRangeLower (w1 + w2)
