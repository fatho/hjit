{-| This module contains the memory management related types. -}
module HJit.Memory.Types
 ( -- * Memory access descriptions
   Access
 , accessNone
 , accessRead
 , accessWrite
 , accessExec
 , canRead
 , canWrite
 , canExecute
 -- * Memory regions
 , Region (..)
 , regionSize
 , regionPtr
 ) where

import           Control.Lens

import           Foreign.C.Types (CSize)
import           Foreign.Ptr     (Ptr)

-- | Describes the allowed access to a memory region.
data Access = Access
  { _accessRead  :: Bool
  -- ^ is reading the memory allowed?
  , _accessWrite :: Bool
  -- ^ is writing the memory allowed?
  , _accessExec  :: Bool
  -- ^ is executing the memory allowed?
  } deriving (Eq, Ord, Show, Read)

accessNone :: Access
accessNone = Access False False False

-- | Access description allowing only read access.
accessRead :: Access
accessRead = Access True False False

-- | Access description allowing only write access.
accessWrite :: Access
accessWrite = Access False True False

-- | Access description allowing only execute access.
accessExec :: Access
accessExec = Access False False True

-- | Lens for getting/setting read access.
canRead :: Lens' Access Bool
canRead = lens _accessRead (\acc x -> acc { _accessRead = x })

-- | Lens for getting/setting write access.
canWrite :: Lens' Access Bool
canWrite = lens _accessWrite (\acc x -> acc { _accessWrite = x })

-- | Lens for getting/setting execute access.
canExecute :: Lens' Access Bool
canExecute = lens _accessExec (\acc x -> acc { _accessExec = x })

instance Monoid Access where
  mempty = accessNone
  mappend (Access ar aw ax) (Access br bw bx) =
    Access (ar || br) (aw || bw) (ax || bx)

-- | A memory address together with the size of the allocated area.
data Region a = Region !(Ptr a) !CSize
  deriving (Eq, Ord, Show)

-- | Return the size of the region in bytes.
regionSize :: Region a -> Int
regionSize (Region _ size) = fromIntegral size

-- | Return the pointer to the region.
regionPtr :: Region a -> Ptr a
regionPtr (Region ptr _) = ptr
