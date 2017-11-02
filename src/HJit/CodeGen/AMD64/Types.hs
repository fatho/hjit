{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
{-| Contains general types that are used throughout the AMD64 code generation. -}
module HJit.CodeGen.AMD64.Types where

import           Data.Word

-- | The word data types that are use on assembly level.
data WordSize
  = W8
  -- ^ 8 bit word
  | W16
  -- ^ 16 bit word
  | W32
  -- ^ 32 bit word
  | W64
  -- ^ 64 bit word
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

type W8 = 'W8
type W16 = 'W16
type W32 = 'W32
type W64 = 'W64

class WordSized (s :: WordSize) where
  type WordTy s :: *
  wordSizeOf :: a s -> WordSize
instance WordSized W8 where
  type WordTy W8 = Word8
  wordSizeOf _ = W8
instance WordSized W16 where
  type WordTy W16 = Word16
  wordSizeOf _ = W16
instance WordSized W32 where
  type WordTy W32 = Word32
  wordSizeOf _ = W32
instance WordSized W64 where
  type WordTy W64 = Word64
  wordSizeOf _ = W64

type Scale = Word8
