{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.WB.Arm.ArmStatic(
  ArmStatic
, HasArmStatic(..)
, HasArmStatics(..)
, SetArmStatic(..)
, HasArmStatic0(..)
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

newtype ArmStatic =
  ArmStatic
    Rational
  deriving (Eq, Ord, Show)

makeClassy ''ArmStatic

class HasArmStatics a where
  armStatics ::
    Traversal'
      a
      ArmStatic

instance HasArmStatics ArmStatic where
  armStatics =
    armStatic

class SetArmStatic a where
  setArmStatic ::
    Setter'
      a
      ArmStatic

instance SetArmStatic ArmStatic where
  setArmStatic =
    armStatic

class HasArmStatic0 a where
  armStatic0 ::
    Lens'
      a
      (Maybe ArmStatic)

instance Inches ArmStatic where
  inches =
    iso
      ArmStatic
      (\(ArmStatic x) -> x)

instance Thouinches ArmStatic where
  thouinches =
    multiplying 1000 . inches

instance Centimetres ArmStatic where
  centimetres =
    dividing (254 % 100) . inches

instance Semigroup ArmStatic where
  (<>) =
    mappend

instance Monoid ArmStatic where
  mempty =
    ArmStatic 0
  ArmStatic w1 `mappend` ArmStatic w2 =
    ArmStatic (w1 + w2)
