{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           System.Exit
import           Test.QuickCheck.All
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Control.DeepSeq
import           Control.Exception (SomeException, try, evaluate)
import           System.IO.Unsafe (unsafePerformIO)

import           Data.Array.IArray
import qualified Data.Array as A
import qualified Data.Array.Diff as D

main :: IO ()
main = do
    runTests <- $(quickCheckAll)
    if runTests then exitSuccess else exitFailure

------------------------------------------------------------------------
-- ** IArray

-- prop_functor = fmap (+ 1) `eq_` fmap (+ 1)

prop_bounds = bounds `eq` bounds

prop_elemAt i = (! i) `eq` (! i)

prop_indices = indices `eq` indices

prop_elems = elems `eq` elems

prop_assocs = assocs `eq` assocs

prop_update ies = (// ies) `eq_` (// ies)

prop_accum ies = accum' `eq_` accum'
  where
    accum' xs = accum (+) xs ies

prop_amap = amap (+1) `eq_` amap (+1)

prop_ixmap ii xs = (ixmap ii go `eq_` ixmap ii go) xs
  where
    go (Ix i) = Ix (i+1)

------------------------------------------------------------------------
-- * Model

newtype Index = Ix Int
  deriving (Eq, Ord, Show, Ix, NFData)

instance Arbitrary Index where
    arbitrary = sized $ \n -> Ix `fmap` choose (0, n)
    shrink (Ix i) = map Ix (shrinkIntegral i)

type Value = Int

type DiffArray = D.DiffArray Index Value

type Model = A.Array Index Value

-- | Check that a function operating on a 'DiffArray is equivalent to
-- one operating on a 'Model'.
eq :: (NFData a, Eq a) => (Model -> a) -> (DiffArray -> a) -> [Value] -> Bool
eq f g xs = f (fromList xs) === g (fromList xs)

eq_ :: (Model -> Model) -> (DiffArray -> DiffArray) -> [Value] -> Bool
eq_ f g = (assocs . f) `eq` (assocs . g)

------------------------------------------------------------------------
-- Utils

fromList :: (IArray a Value) => [Value] -> a Index Value
fromList [] = array (Ix 1, Ix 0) []
fromList xs = listArray (Ix 0, Ix n) xs
  where
    n = length xs - 1

-- | Equality as defined by Eq, but also allows _|_ == _|_
(===) :: (NFData a, Eq a) => a -> a -> Bool
(===) x y | isBottom x && isBottom y = True
          | otherwise                = x == y

isBottom :: NFData a => a -> Bool
isBottom x = unsafePerformIO $ do
    x' <- (try . evaluate . force) x
    case x' of
      Left (_::SomeException) -> return True
      Right _                 -> return False
