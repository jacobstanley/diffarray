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
      DiffUArray
    , module Data.Array.IArray
    ) where

------------------------------------------------------------------------
-- Imports.

import Data.Array.Base (IArray(..), MArray(..), showsIArray)
import Data.Array.IO (IOUArray)
import Data.Array.IArray

-- import Foreign.Ptr        (Ptr, FunPtr)
-- import Foreign.StablePtr  (StablePtr)
-- import Data.Int           (Int8,  Int16,  Int32,  Int64)
-- import Data.Word          (Word, Word8, Word16, Word32, Word64)

import Data.IORef         (IORef, newIORef, readIORef, writeIORef)
import Control.Monad      (foldM)

import System.IO.Unsafe   (unsafePerformIO)

------------------------------------------------------------------------
-- DiffUArray Type

newtype DiffUArray i e = DiffUArray (IORef (Data i e))

class (Ix i, MArray IOUArray e IO) => DUA i e where
    data Data i e
    dArray :: IOUArray   i e             -> Data i e
    dDiff  :: DiffUArray i e -> Int -> e -> Data i e
    unpackData :: Data i e
               -> (IOUArray   i e             -> b)
               -> (DiffUArray i e -> Int -> e -> b)
               -> b

-- class IArray a e where
--   bounds           :: Ix i => a i e -> (i,i)
--   numElements      :: Ix i => a i e -> Int
--   unsafeArray      :: Ix i => (i,i) -> [(Int, e)] -> a i e
--   unsafeAt         :: Ix i => a i e -> Int -> e
--   unsafeReplace    :: Ix i => a i e -> [(Int, e)] -> a i e
--   unsafeAccum      :: Ix i => (e -> e' -> e) -> a i e -> [(Int, e')] -> a i e
--   unsafeAccumArray :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> a i e

instance (Ix i) => DUA i Int where
    data Data i Int = IntArray !(IOUArray   i Int)
                    | IntDiff  !(DiffUArray i Int) !Int !Int
    dArray a     = IntArray a
    dDiff  a j e = IntDiff  a j e
    unpackData (IntArray a)     f _ = f a
    unpackData (IntDiff  a j e) _ f = f a j e

instance IArray DiffUArray Int where
    bounds        a      = unsafePerformIO (boundsDiffArray a)
    numElements   a      = unsafePerformIO (numElementsDiffArray a)
    unsafeArray   lu ies = unsafePerformIO (newDiffArray lu ies)
    unsafeAt      a i    = unsafePerformIO (readDiffArray a i)
    unsafeReplace a ies  = unsafePerformIO (replaceDiffArray a ies)

instance (Ix ix, Show ix) => Show (DiffUArray ix Int) where
  showsPrec = showsIArray

------------------------------------------------------------------------

withRef :: (DUA i e)
       => DiffUArray i e
       -> (IOUArray   i e             -> IO b)
       -> (DiffUArray i e -> Int -> e -> IO b)
       -> IO b
withRef (DiffUArray ref) f g = do
    d <- readIORef ref
    unpackData d f g

writeRef :: (DUA i e) => DiffUArray i e -> Data i e -> IO ()
writeRef (DiffUArray ref) d = writeIORef ref d

mkDiffArray :: (DUA i e) => IOUArray i e -> IO (DiffUArray i e)
mkDiffArray ua = do
    ref <- newIORef (dArray ua)
    return (DiffUArray ref)

reroot :: (DUA i e) => DiffUArray i e -> IO ()
reroot da = withRef da arry diff
  where
    arry _       = return ()
    diff da' i e = afterReroot da' $ \ua -> do
        -- Replace value in IOUArray
        e' <- unsafeRead ua i
        unsafeWrite ua i e
        -- Switch Diff/Array nodes
        writeRef da  (dArray ua)
        writeRef da' (dDiff da i e')

afterReroot :: (DUA i e) => DiffUArray i e -> (IOUArray i e -> IO b) -> IO b
afterReroot da f = withRef da f go
  where
    go _ _ _ = reroot da >> withRef da f (error "afterReroot: reroot failed")

------------------------------------------------------------------------

boundsDiffArray :: (DUA i e) => DiffUArray i e -> IO (i, i)
boundsDiffArray da = afterReroot da getBounds

numElementsDiffArray :: (DUA i e) => DiffUArray i e -> IO Int
numElementsDiffArray da = afterReroot da getNumElements

newDiffArray :: (DUA i e) => (i, i) -> [(Int, e)] -> IO (DiffUArray i e)
newDiffArray lu ies = do
    a <- newArray_ lu
    sequence_ [unsafeWrite a i e | (i, e) <- ies]
    mkDiffArray a

readDiffArray :: (DUA i e) => DiffUArray i e -> Int -> IO e
readDiffArray da i = afterReroot da ((flip unsafeRead) i)

writeDiffArray :: (DUA i e) => DiffUArray i e -> Int -> e -> IO (DiffUArray i e)
writeDiffArray old i e = afterReroot old $ \ua -> do
    -- Replace value in IOUArray
    e' <- unsafeRead ua i
    unsafeWrite ua i e
    -- Make new array and write reference in to old array
    new <- mkDiffArray ua
    writeRef old (dDiff new i e')
    return new

replaceDiffArray :: (DUA i e) => DiffUArray i e -> [(Int, e)] -> IO (DiffUArray i e)
replaceDiffArray = foldM (\da (i, e) -> writeDiffArray da i e)

------------------------------------------------------------------------
-- Show instances

--instance (Ix ix, Show ix, Show e) => Show (DiffArray ix e) where
--  showsPrec = showsIArray

--instance (Ix ix, Show ix) => Show (DiffUArray ix Bool) where
--  showsPrec = showsIArray
--
--instance (Ix ix, Show ix) => Show (DiffUArray ix Char) where
--  showsPrec = showsIArray

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
