{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{- | Contains the types representing AMD64 assembly code in Haskell, and their
   (smart-)constructors. -}
module HJit.CodeGen.AMD64.Types where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Fix
import           Control.Monad.State.Strict
import           Data.Bits                  (Bits (..))
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Word                  (Word16, Word32, Word64, Word8)
import           Foreign.Ptr                (IntPtr)

import           HJit.CodeGen.Assembler

-- | Defines the errors that can occur in the assembler
data AMD64Error
  = InvalidInstruction Text
    -- ^ the desired instruction is not available
  deriving (Show, Eq)

-- | AMD64 assembler monad
type AMD64 = Assembler AMD64Error

-- | Class of things that can be encoded into AMD64 assembly code.
class ToAMD64 a where
  -- | Generate the opcode for the given @a@.
  opcode :: a -> AMD64 ()

-- * Operands

-- | A general purpose register annotated with its size
data GPReg (s :: IntegralSize) = GPReg {-# UNPACK #-} !IntegralSize {-# UNPACK #-} !Word8

-- | The data types that are use on assembly level
data IntegralSize
  = Size8
  -- ^ 8 bit word
  | Size16
  -- ^ 16 bit word
  | Size32
  -- ^ 32 bit word
  | Size64


-- * Instruction Encoding

-- | REX prefix byte. This type encodes the invariant that the highest 4 bits are always 0x4.
newtype REX = REX { getRex :: Word8 }

instance Monoid REX where
  mempty = REX 0x40
  mappend (REX a) (REX b) = REX (a .|. b)

instance ToAMD64 REX where
  opcode = word8LE . getRex

rexW, rexR, rexX, rexB :: REX
rexW = REX 0x48
rexR = REX 0x44
rexX = REX 0x42
rexB = REX 0x41

-- | ModRM byte.
newtype ModRM = ModRM { getModRM :: Word8 }

instance Monoid ModRM where
  mempty = ModRM 0
  mappend (ModRM a) (ModRM b) = ModRM (a .|. b)

instance ToAMD64 ModRM where
  opcode = word8LE . getModRM

-- TODO: documentation about all these special bytes, see http://wiki.osdev.org/X86-64_Instruction_Encoding

mod00 :: ModRM
mod00 = ModRM 0x00

mod01 :: ModRM
mod01 = ModRM 0x40

mod10 :: ModRM
mod10 = ModRM 0x80

mod11 :: ModRM
mod11 = ModRM 0xC0

modReg :: Word8 -> ModRM
modReg reg = ModRM $ (reg .&. 0x7) `shiftL` 3

modRM :: Word8 -> ModRM
modRM rm = ModRM $ rm .&. 0x7


-- | TODO: SIB byte
newtype SIB = SIB { getSIP :: Word8 }
