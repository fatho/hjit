{-# LANGUAGE CPP #-}
{-| This module provides a platform independent interface to memory management. -}
module HJit.Memory
    ( module Types
    , allocateRegion
    , freeRegion
    , protectRegion
    ) where

#if defined(OS_Linux) || defined(OS_Darwin)
import           HJit.Memory.POSIX (allocateRegion, freeRegion, protectRegion)
#endif
import           HJit.Memory.Types as Types
