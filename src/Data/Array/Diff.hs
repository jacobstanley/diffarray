{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Diff
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.IArray)
--
-- Functional arrays with constant-time update.
--
-----------------------------------------------------------------------------

module Data.Array.Diff (

    -- * Diff array types

    -- | Diff arrays have an immutable interface, but rely on internal
    -- updates in place to provide fast functional update operator
    -- '//'.
    --
    -- When the '//' operator is applied to a diff array, its contents
    -- are physically updated in place. The old array silently changes
    -- its representation without changing the visible behavior:
    -- it stores a link to the new current array along with the
    -- difference to be applied to get the old contents.
    --
    -- So if a diff array is used in a single-threaded style,
    -- i.e. after '//' application the old version is no longer used,
    -- @a'!'i@ takes O(1) time and @a '//' d@ takes O(@length d@).
    -- Accessing elements of older versions gradually becomes slower.
    --
    -- Updating an array which is not current makes a physical copy.
    -- The resulting array is unlinked from the old family. So you
    -- can obtain a version which is guaranteed to be current and
    -- thus have fast element access by @a '//' []@.

    -- Possible improvement for the future (not implemented now):
    -- make it possible to say "I will make an update now, but when
    -- I later return to the old version, I want it to mutate back
    -- instead of being copied".

    -- IOToDiffArray, -- data IOToDiffArray
                   --     (a :: * -> * -> *) -- internal mutable array
                   --     (i :: *)           -- indices
                   --     (e :: *)           -- elements

    -- | Type synonyms for the two most important IO array types.

    -- Two most important diff array types are fully polymorphic
    -- lazy boxed DiffArray:
    -- DiffArray,     -- = IOToDiffArray IOArray
    -- ...and strict unboxed DiffUArray, working only for elements
    -- of primitive types but more compact and usually faster:
    DiffUArray,    -- = IOToDiffArray IOUArray

    -- * Overloaded immutable array interface

    -- | Module "Data.Array.IArray" provides the interface of diff arrays.
    -- They are instances of class 'IArray'.
    module Data.Array.IArray,

    -- * Low-level interface

    -- | These are really internal functions, but you will need them
    -- to make further 'IArray' instances of various diff array types
    -- (for either more 'MArray' types or more unboxed element types).
    newDiffArray, readDiffArray, replaceDiffArray
    )
    where

------------------------------------------------------------------------
-- Imports.

import Data.Array.Base
import Data.Array.IArray
import Data.Array.IO (IOArray, IOUArray)

import Foreign.Ptr        (Ptr, FunPtr)
import Foreign.StablePtr  (StablePtr)
import Data.Int           (Int8,  Int16,  Int32,  Int64)
import Data.Word          (Word, Word8, Word16, Word32, Word64)
import Data.IORef         (IORef)
import Data.IORef         (newIORef, readIORef, writeIORef)
import Control.Monad      (foldM)

import System.IO.Unsafe   (unsafePerformIO)

------------------------------------------------------------------------
-- Diff array types.

-- | An arbitrary 'MArray' type living in the 'IO' monad can be converted
-- to a diff array.

newtype DiffUArray ix e =
    DiffUArray {varDiffArray :: IORef (DiffData ix e)}

class DA ix e where
    data DiffData ix e
    current :: IOUArray ix e -> DiffData ix e
    diff    :: DiffUArray ix e -> Int -> e -> DiffData ix e
    withDA  :: DiffData ix e -> (IOUArray ix e -> b) -> (DiffUArray ix e -> Int -> e -> b) -> b

instance (Ix ix) => DA ix Int where
    data DiffData ix Int = CurrentInt !(IOUArray ix Int)
                         | DiffInt !(DiffUArray ix Int) !Int !Int
    current a     = CurrentInt a
    diff    a i e = DiffInt a i e
    withDA (CurrentInt a) f _  = f a
    withDA (DiffInt a i e) _ f = f a i e

-- Internal representation: either a mutable array, or a link to
-- another diff array patched with a list of index+element pairs.
--data instance DiffArrayData i e = Current !(IOUArray i e)
--                       | Diff !(DiffUArray i e) !Int !e

-- | Fully polymorphic lazy boxed diff array.
--type DiffArray  = IOToDiffArray IOArray

-- | Strict unboxed diff array, working only for elements
-- of primitive types but more compact and usually faster than 'DiffArray'.
--type DiffUArray = IOToDiffArray IOUArray

-- Having 'MArray a e IO' in instance context would require
-- -XUndecidableInstances, so each instance is separate here.

------------------------------------------------------------------------
-- Show instances

--instance (Ix ix, Show ix, Show e) => Show (DiffArray ix e) where
--  showsPrec = showsIArray

--instance (Ix ix, Show ix) => Show (DiffUArray ix Bool) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Char) where
--  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Int) where
  showsPrec = showsIArray

--instance (Ix ix, Show ix) => Show (DiffUArray ix Word) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Float) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Double) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Int8) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Int16) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Int32) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Int64) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Word8) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Word16) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Word32) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Word64) where
--  showsPrec = showsIArray

------------------------------------------------------------------------
-- IArray instances

--instance IArray (IOToDiffArray IOArray) e where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}

--instance IArray (DiffUArray) Bool where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Char where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}

instance IArray DiffUArray Int where
    bounds        a      = unsafePerformIO $ boundsDiffArray a
    {-# NOINLINE bounds #-}
    numElements   a      = unsafePerformIO $ numElementsDiffArray a
    {-# NOINLINE numElements #-}
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    {-# NOINLINE unsafeArray #-}
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    {-# NOINLINE unsafeAt #-}
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
    {-# NOINLINE unsafeReplace #-}

--instance IArray (DiffUArray) Word where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) (Ptr a) where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) (FunPtr a) where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Float where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Double where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) (StablePtr a) where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Int8 where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Int16 where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Int32 where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Int64 where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Word8 where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Word16 where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Word32 where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}
--
--instance IArray (DiffUArray) Word64 where
--    bounds        a      = unsafePerformIO $ boundsDiffArray a
--    {-# NOINLINE bounds #-}
--    numElements   a      = unsafePerformIO $ numElementsDiffArray a
--    {-# NOINLINE numElements #-}
--    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
--    {-# NOINLINE unsafeArray #-}
--    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
--    {-# NOINLINE unsafeAt #-}
--    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray` ies
--    {-# NOINLINE unsafeReplace #-}

------------------------------------------------------------------------
-- The important stuff.

newDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
             => (i,i)
             -> [(Int, e)]
             -> IO (DiffUArray i e)
newDiffArray (l,u) ies = do
    a <- newArray_ (l,u)
    sequence_ [unsafeWrite a i e | (i, e) <- ies]
    var <- newIORef $! current a
    return $! DiffUArray var

readDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
              => DiffUArray i e
              -> Int
              -> IO e
a `readDiffArray` i = do
    d <- readIORef (varDiffArray a)
    withDA d (\a' -> unsafeRead a' i)
             (\a' i' e -> if i == i' then return e
                                   else readDiffArray a' i)

replaceDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
                => DiffUArray i e
                -> [(Int, e)]
                -> IO (DiffUArray i e)
replaceDiffArray = foldM replaceDiffArray1

replaceDiffArray1 :: (MArray IOUArray e IO, Ix i, DA i e)
                 => DiffUArray i e
                 -> (Int, e)
                 -> IO (DiffUArray i e)
replaceDiffArray1 a (i, e) = do
    d <- readIORef (varDiffArray a)
    withDA d update copy
  where
    update a' = do
        -- Replace value in array
        e' <- unsafeRead a' i
        unsafeWrite a' i e
        -- Update old IORef to point to new Current
        var' <- newIORef $! current a'
        writeIORef (varDiffArray a) $! diff (DiffUArray var') i e'
        return $! DiffUArray var'
    copy _ _ _ = do
        -- We still do the copy when there is nothing to change
        -- but this is not the current version. So you can use
        -- 'a // []' to make sure that the resulting array has
        -- fast element access.
        a' <- thawDiffArray a
        -- thawDiffArray gives a fresh array which we can safely
        -- mutate.
        unsafeWrite a' i e
        var' <- newIORef $! current a'
        return $! DiffUArray var'


boundsDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
                => DiffUArray i e
                -> IO (i,i)
boundsDiffArray a = do
    d <- readIORef (varDiffArray a)
    withDA d getBounds (\a' _ _ -> boundsDiffArray a')

numElementsDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
                     => DiffUArray i e
                     -> IO Int
numElementsDiffArray a = do
    d <- readIORef (varDiffArray a)
    withDA d getNumElements (\a' _ _ -> numElementsDiffArray a')

freezeDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
                => IOUArray i e
                -> IO (DiffUArray i e)
freezeDiffArray a = do
  (l,u) <- getBounds a
  a' <- newArray_ (l,u)
  sequence_ [unsafeRead a i >>= unsafeWrite a' i | i <- [0 .. rangeSize (l,u) - 1]]
  var <- newIORef $! current a'
  return $! DiffUArray var

-- {-# RULES
-- "freeze/DiffUArray" freeze = freezeDiffArray
--     #-}

-- unsafeFreezeDiffArray is really unsafe. Better don't use the old
-- array at all after freezing. The contents of the source array will
-- be changed when '//' is applied to the resulting array.

unsafeFreezeDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
                      => IOUArray i e
                      -> IO (DiffUArray i e)
unsafeFreezeDiffArray a = do
    var <- newIORef $! current a
    return $! DiffUArray var

-- {-# RULES
-- "unsafeFreeze/DiffUArray" unsafeFreeze = unsafeFreezeDiffArray
--     #-}

thawDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
              => DiffUArray i e
              -> IO (IOUArray i e)
thawDiffArray a = do
    d <- readIORef (varDiffArray a)
    withDA d copy go
  where
    copy a' = do
        (l,u) <- getBounds a'
        a'' <- newArray_ (l,u)
        sequence_ [unsafeRead a' i >>= unsafeWrite a'' i | i <- [0 .. rangeSize (l,u) - 1]]
        return a''
    go a' i e = do
        a'' <- thawDiffArray a'
        unsafeWrite a'' i e
        return a''

-- {-# RULES
-- "thaw/DiffUArray" thaw = thawDiffArray
--     #-}

-- unsafeThawDiffArray is really unsafe. Better don't use the old
-- array at all after thawing. The contents of the resulting array
-- will be changed when '//' is applied to the source array.

unsafeThawDiffArray :: (MArray IOUArray e IO, Ix i, DA i e)
                    => DiffUArray i e
                    -> IO (IOUArray i e)
unsafeThawDiffArray a = do
    d <- readIORef (varDiffArray a)
    withDA d return go
  where
    go a' i e = do
        a'' <- unsafeThawDiffArray a'
        unsafeWrite a'' i e
        return a''

-- {-# RULES
-- "unsafeThaw/DiffUArray" unsafeThaw = unsafeThawDiffArray
--    #-}
