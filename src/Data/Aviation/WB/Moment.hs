{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aviation.WB.Moment(
  Moment(..)
, HasMoment(..)
, HasMoments(..)
, SetMoment(..)
, HasMoment0(..)
, totalMoment
, totalMoments
, totalMoment''
, totalWeights
, momentX
) where

import Control.Applicative(Applicative((<*>)))
import Control.Category((.))
import Control.Lens(Lens', Traversal', Setter', Iso', lens, makeClassy, view, re)
import Data.Aviation.WB.Arm.ArmStatic(ArmStatic, HasArmStatic(armStatic), HasArmStatics(armStatics), SetArmStatic(setArmStatic))
import Data.Aviation.WB.Weight(Weight, HasWeight(weight), HasWeights(weights), SetWeight(setWeight))
import Data.Eq(Eq)
import Data.Foldable(Foldable, foldl, foldMap)
import Data.Functor((<$>))
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Prelude(Show, (*), (+), Rational)

data Moment =
  Moment
    Weight
    ArmStatic
  deriving (Eq, Ord, Show)

makeClassy ''Moment

class HasMoments a where
  moments ::
    Traversal'
      a
      Moment

instance HasMoments Moment where
  moments =
    moment

class SetMoment a where
  setMoment ::
    Setter'
      a
      Moment

instance SetMoment Moment where
  setMoment =
    moment

class HasMoment0 a where
  moment0 ::
    Lens'
      a
      (Maybe Moment)

instance HasWeight Moment where
  weight =
    lens
      (\(Moment w _) -> w)
      (\(Moment _ a) w -> Moment w a)

instance HasWeights Moment where
  weights =
    weight

instance SetWeight Moment where
  setWeight =
    weight

instance HasArmStatic Moment where
  armStatic =
    lens
      (\(Moment _ a) -> a)
      (\(Moment w _) a -> Moment w a)

instance HasArmStatics Moment where
  armStatics =
    armStatic

instance SetArmStatic Moment where
  setArmStatic =
    armStatic

totalMoment ::
  HasMoment moment =>
  Iso' Rational Weight
  -> Iso' Rational ArmStatic
  -> moment
  -> Rational
totalMoment w a m =
  let r = view (moment . weight . re w) m
      s = view (moment . armStatic . re a) m
  in  r * s
  
totalMoments ::
  (HasMoment moment, Foldable f) =>
  Iso' Rational Weight
  -> Iso' Rational ArmStatic
  -> f moment
  -> Rational
totalMoments w a =
  foldl (\x y -> x + totalMoment w a y) 0


totalMoment'' ::
  (HasMoment moment, Foldable f) =>
  Iso' Rational Weight
  -> Iso' Rational ArmStatic
  -> f moment
  -> Moment
totalMoment'' w a m =
  let (mw, ma) = foldl (\(w', a') n ->  let w'' = view (moment . weight . re w) n
                                            a'' = view (moment . armStatic . re a) n
                                        in  (w' + w'', a' + w'' * a'')) (0, 0) m
  in  Moment (view w mw) (view a ma)

totalWeights ::
  (HasMoment moment, Foldable f) =>
  f moment
  -> Weight
totalWeights =
  foldMap (view (moment . weight))
    
momentX :: 
  (HasWeight w, HasArmStatic s, Applicative f) =>
  f w
  -> f s
  -> f Moment
momentX wt b =
  (\w -> Moment (view weight w) . view armStatic) <$> wt <*> b
