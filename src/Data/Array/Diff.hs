{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- | Functional arrays with constant-time update.

module Data.Array.Diff (
      DiffUArray
    , module Data.Array.IArray
    ) where

------------------------------------------------------------------------
-- Imports.

import Data.Array.Base   (IArray(..), MArray(..), showsIArray)
import Data.Array.IO     (IOUArray)
import Data.Array.IArray

import Control.Monad    (foldM)
import Data.IORef       (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr       (Ptr, FunPtr)
import Foreign.StablePtr (StablePtr)
import Data.Int          (Int8,  Int16,  Int32,  Int64)
import Data.Word         (Word, Word8, Word16, Word32, Word64)

------------------------------------------------------------------------
-- Types

newtype DiffUArray i e = DiffUArray (IORef (Data i e))

class (Ix i, MArray IOUArray e IO) => DUA i e where
    data Data i e
    dArray :: IOUArray   i e             -> Data i e
    dDiff  :: DiffUArray i e -> Int -> e -> Data i e
    unpackData :: Data i e
               -> (IOUArray   i e             -> b)
               -> (DiffUArray i e -> Int -> e -> b)
               -> b

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
-- Instances

#define s(z) z

#define diffData(con,ty)                                            \
instance (Ix i) => DUA i (ty) where {                               \
  data Data i (ty) = s(con)Array !(IOUArray   i (ty))               \
                   | s(con)Diff  !(DiffUArray i (ty)) !Int !(ty)    \
; dArray a     = s(con)Array a                                      \
; dDiff  a j e = s(con)Diff  a j e                                  \
; unpackData (s(con)Array a)     f _ = f a                          \
; unpackData (s(con)Diff  a j e) _ g = g a j e }                    \

#define diffIArray(ty)                                              \
instance IArray DiffUArray (ty) where {                             \
  bounds        a      = unsafePerformIO (boundsDiffArray a)        \
; numElements   a      = unsafePerformIO (numElementsDiffArray a)   \
; unsafeArray   lu ies = unsafePerformIO (newDiffArray lu ies)      \
; unsafeAt      a i    = unsafePerformIO (readDiffArray a i)        \
; unsafeReplace a ies  = unsafePerformIO (replaceDiffArray a ies) } \

#define diffShow(ty)                                                \
instance (Ix i, Show i) => Show (DiffUArray i (ty)) where {         \
  showsPrec = showsIArray }

diffData(Int,Int)
diffIArray(Int)
diffShow(Int)

diffData(Int8,Int8)
diffIArray(Int8)
diffShow(Int8)

diffData(Int16,Int16)
diffIArray(Int16)
diffShow(Int16)

diffData(Int32,Int32)
diffIArray(Int32)
diffShow(Int32)

diffData(Int64,Int64)
diffIArray(Int64)
diffShow(Int64)

diffData(Word,Word)
diffIArray(Word)
diffShow(Word)

diffData(Word8,Word8)
diffIArray(Word8)
diffShow(Word8)

diffData(Word16,Word16)
diffIArray(Word16)
diffShow(Word16)

diffData(Word32,Word32)
diffIArray(Word32)
diffShow(Word32)

diffData(Word64,Word64)
diffIArray(Word64)
diffShow(Word64)

diffData(Ptr,Ptr a)
diffIArray(Ptr a)
diffShow(Ptr a)

diffData(FunPtr,FunPtr a)
diffIArray(FunPtr a)
diffShow(FunPtr a)

diffData(StablePtr,StablePtr a)
diffIArray(StablePtr a)
