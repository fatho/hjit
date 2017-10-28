{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-| This module contains the POSIX implementation of memory management. -}
module HJit.Memory.POSIX
  ( allocateRegion
  , freeRegion
  , protectRegion
  ) where

import           Control.Lens      ((^.))

import           Data.Bits         ((.|.))
import           Foreign.C         (CInt (..), CPtrdiff (..), CSize (..))
import           Foreign.C.Error   (throwErrnoIf, throwErrnoIfMinus1_)
import           Foreign.Ptr       (Ptr, nullPtr, ptrToIntPtr)

import           HJit.Memory.Types

-- | Allocate a region of the given size and the given access protection.
allocateRegion :: Int -> Access -> IO (Region a)
allocateRegion size access = do
  let prot = memoryAccessToProtectionFlags access
      flags = MAP_PRIVATE .|. MAP_ANONYMOUS
      isMapInvalid ptr = ptrToIntPtr ptr == -1
  bufPtr <- throwErrnoIf isMapInvalid "mmap" $ mmap nullPtr (fromIntegral size) prot flags (-1) 0
  return $ Region bufPtr (fromIntegral size)

-- | Free a region previously allocated with 'allocateRegion'
freeRegion :: Region a -> IO ()
freeRegion (Region ptr size) = throwErrnoIfMinus1_ "munmap" $ munmap ptr size

-- | Change the memory protection flags for the given region.
protectRegion :: Access -> Region a -> IO ()
protectRegion access (Region ptr size) = throwErrnoIfMinus1_ "mprotect" $
  mprotect ptr size (memoryAccessToProtectionFlags access)

-- * POSIX implementation details

foreign import ccall "sys/mman.h" mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> CPtrdiff -> IO (Ptr a)
foreign import ccall "sys/mman.h" munmap :: Ptr a -> CSize -> IO CInt
foreign import ccall "sys/mman.h" mprotect :: Ptr a -> CSize -> CInt -> IO CInt

pattern PROT_EXEC :: CInt
pattern PROT_EXEC = 1
pattern PROT_WRITE :: CInt
pattern PROT_WRITE = 2
pattern PROT_READ :: CInt
pattern PROT_READ = 4

pattern MAP_PRIVATE :: CInt
pattern MAP_PRIVATE = 2
pattern MAP_ANONYMOUS :: CInt
pattern MAP_ANONYMOUS = 0x20

-- | Convert the 'Access' description into POSIX memory protection flags.
memoryAccessToProtectionFlags :: Access -> CInt
memoryAccessToProtectionFlags access = prot where
  prot = (PROT_READ `ifSet` canRead) .|. (PROT_WRITE `ifSet` canWrite) .|. (PROT_EXEC `ifSet` canExecute)
  ifSet flag accessor = if access ^. accessor then flag else 0

