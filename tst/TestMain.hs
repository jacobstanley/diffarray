{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           System.Exit
import           Test.QuickCheck
import           Test.QuickCheck.All

import           Data.Array.IArray
import qualified Data.Array as A
import qualified Data.Array.Diff as D

main :: IO ()
main = do
    runTests <- $(quickCheckAll)
    if runTests then exitSuccess else exitFailure

------------------------------------------------------------------------
-- ** Instances

--pFunctor :: [Int] -> Bool
--pFunctor = fmap (+ 1) `eq` fmap (+ 1)

-- Returns the element of an immutable array at the specified index.
-- (!) :: (IArray a e, Ix i) => a i e -> i -> eSource

prop_elemAt :: [Int] -> Property
prop_elemAt xs =
    not (null xs) ==> forAll (indexOf xs) $ \i -> ((! i) `eq` (! i)) xs

-- indices :: (IArray a e, Ix i) => a i e -> [i]Source
-- Returns a list of all the valid indices in an array.

prop_indices :: [Int] -> Bool
prop_indices = indices `eq` indices

-- elems :: (IArray a e, Ix i) => a i e -> [e]Source
-- Returns a list of all the elements of an array, in the same order as their indices.

prop_elems :: [Int] -> Bool
prop_elems = elems `eq` elems

-- assocs :: (IArray a e, Ix i) => a i e -> [(i, e)]Source
-- Returns the contents of an array as a list of associations.

prop_assocs :: [Int] -> Bool
prop_assocs = assocs `eq` assocs

-- (//) :: (IArray a e, Ix i) => a i e -> [(i, e)] -> a i e

--prop_replace :: [Int] -> Property
--prop_replace xs =
--    not (null xs) ==> forAll (indicesOf xs) $ \(is :: [(Int,Int)]) ->
--    (toList . (// is)) `eq` (toList . (// is))

indexOf :: [a] -> Gen Int
indexOf xs = elements [0..n-1]
  where
    n = length xs

--indicesOf :: Arbitrary a => [a] -> Gen [(Int, a)]
--indicesOf xs = do
--    is <- listOf1 (indexOf xs)
--    vs <- vector (length is)
--    return (zip is vs)

------------------------------------------------------------------------
-- * Model

type Index = Int

type DiffArray a = D.DiffArray Index a

type Model a = A.Array Index a

-- | Check that a function operating on a 'DiffArray is equivalent to
-- one operating on a 'Model'.
eq :: (Eq a, Eq b, ToList c a)
   => (Model a -> b)      -- ^ Function that modifies a 'Model' in the same
                          -- way
   -> (DiffArray a -> b)  -- ^ Function that modified a 'DiffArray
   -> c                   -- ^ Initial content of the 'DiffArray and 'Model'
   -> Bool                -- ^ True if the functions are equivalent
eq f g xs = g (fromList xs') == f (fromList xs')
  where
    xs' = toList xs

------------------------------------------------------------------------
-- Utils

fromList :: (IArray a e) => [e] -> a Int e
fromList [] = array (1,0) []
fromList xs = listArray (0, length xs - 1) xs

class ToList a e | a -> e where
    toList :: a -> [e]

instance ToList [a] a where
    toList xs = xs

instance ToList (NonEmptyList a) a where
    toList (NonEmpty xs) = xs

instance (IArray a e, Ix i) => ToList (a i e) e where
    toList = elems
