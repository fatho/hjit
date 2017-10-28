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
import           Data.Int
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Word                  (Word16, Word32, Word64, Word8)
import           Foreign.Ptr                (IntPtr)

import           HJit.CodeGen.Assembler
import           HJit.Util                  (enableIf)

-- | Defines the errors that can occur in the assembler
data AMD64Error
  = InvalidInstruction Text
    -- ^ the desired instruction is not available
  deriving (Show, Eq)

-- | AMD64 assembler monad
type AMD64 = Assembler AMD64Error

-- | Class of things that can be encoded into AMD64 assembly code.
class ToAMD64 a where
  -- | Generate the AMD64 byte representation for the given @a@.
  emit :: a -> AMD64 ()

-- | Throw an invalid instruction error.
invalidInstruction :: Text -> AMD64 ()
invalidInstruction = throwError . InvalidInstruction

-- | Special case of an invalid instruction error, signifying that AH, CH, DH
-- and BH registers cannot be encoded in operations that require a REX prefix.
cannotEncodeWithRex :: AMD64 ()
cannotEncodeWithRex = invalidInstruction "Cannot encode AH, CH, DH or BH register in an instruction requiring a REX prefix."

-- * Operands

-- | A general purpose register annotated with its size
data Reg (s :: WordSize) = Reg
  { regIndex :: !Word8
    -- ^ The internal index of that register
  , regHigh8 :: !Bool
    -- ^ Determines whether this register accesses the high 8 bit of the lower 16 bit of the (AH, CH, DH, BH).
  }

-- | Unsafely assign a new size to the register type.
unsafeCastReg :: Reg s1 -> Reg s2
unsafeCastReg (Reg idx high8) = Reg idx high8

-- | Instruction pointer register
data IP (s :: WordSize) = IP

-- | Control register
newtype CR = CR Word8

-- | Debug register
newtype DR = DR Word8

type W8 = 'W8
type W16 = 'W16
type W32 = 'W32
type W64 = 'W64

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

class WordSized (s :: WordSize) where
  wordSize :: a s -> WordSize

instance WordSized W8 where
  wordSize _ = W8

instance WordSized W16 where
  wordSize _ = W16

instance WordSized W32 where
  wordSize _ = W32

instance WordSized W64 where
  wordSize _ = W64

-- * Instruction Encoding

data OpCode
  = OpCodeByte !Word8
    -- ^ a single byte opcode
  | OpCodeTwoByte !Word8
    -- ^ a two byte opcode (prefixed with 0x0F)

-- | Apply a transformation to the opcode byte.
mapOpCode :: (Word8 -> Word8) -> OpCode -> OpCode
mapOpCode f (OpCodeByte opcode) = OpCodeByte (f opcode)
mapOpCode f (OpCodeTwoByte opcode) = OpCodeTwoByte (f opcode)

instance ToAMD64 OpCode where
  emit (OpCodeByte b)    = word8 b
  emit (OpCodeTwoByte b) = word8 0x0F >> word8 b

-- | Optional REX prefix byte. This type encodes the invariant that it is either 0 or the highest 4 bits are always 0x4.
newtype REX = REX { getRex :: Word8 }

instance Monoid REX where
  mempty = REX 0x00
  mappend (REX a) (REX b) = REX (a .|. b)

instance ToAMD64 REX where
  emit = word8 . getRex

-- | Should the REX prefix be present. If there should be no REX prefix, this is encoded with 0.
isRexPresent :: REX -> Bool
isRexPresent (REX r) = r /= 0

rexNone, rexW, rexR, rexX, rexB :: REX
rexNone = REX 0x40
rexW = REX 0x48
rexR = REX 0x44
rexX = REX 0x42
rexB = REX 0x41

-- | Check whether the register index needs a REX prefix for encoding.
regIndexNeedsREX :: Reg s -> Bool
regIndexNeedsREX (Reg reg _) = testBit reg 3

-- | Check whether a REX prefix is needed for selecting the correct low 8 bit register.
regNeedsLow8REX :: Reg W8 -> Bool
regNeedsLow8REX (Reg idx False) = idx >= 4 && idx <= 7
regNeedsLow8REX _               = False

-- | Check whether a REX prefix prevents encoding the given register.
regProhibitsREX :: Reg W8 -> Bool
regProhibitsREX (Reg idx True) = idx >= 4 && idx <= 7
regProhibitsREX _              = False

-- | ModRM byte.
newtype ModRM = ModRM { getModRM :: Word8 }

instance Monoid ModRM where
  mempty = ModRM 0
  mappend (ModRM a) (ModRM b) = ModRM (a .|. b)

instance ToAMD64 ModRM where
  emit = word8 . getModRM

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

-- | The scale of an index as a power of two.
type Scale = Word8

-- | SIB byte (scale, index, base)
newtype SIB = SIB { getSIB :: Word8 }

instance Monoid SIB where
  mempty = SIB 0
  mappend (SIB a) (SIB b) = SIB (a .|. b)

instance ToAMD64 SIB where
  emit = word8 . getSIB

sibScale :: Scale -> SIB
sibScale s = SIB $ (s .&. 0x3) `shiftL` 6

sibIndex :: Word8 -> SIB
sibIndex i = SIB $ (i .&. 0x7) `shiftL` 3

sibBase :: Word8 -> SIB
sibBase b = SIB $ (b .&. 0x7)

-- * Operands

data Ind as
  = IndB (Reg as)
    -- ^ [base]
  | IndBD8 (Reg as) Int8
    -- ^ [base + disp8]
  | IndBD32 (Reg as) Int32
    -- ^ [base + disp32]
  | IndBI (Reg as) (Reg as) Scale
    -- ^ [base + index * 2^s]
  | IndBID8 (Reg as) (Reg as) Scale Int8
    -- ^ [base + index * 2^s + disp8]
  | IndBID32 (Reg as) (Reg as) Scale Int32
    -- ^ [base + index * 2^s + disp32]
  | IndID32 (Reg as) Scale Int32
    -- ^ [index * 2^s + disp32]
  | IndIP Int32
    -- ^ [EIP/RIP + disp32]
  | IndD32 Int32
    -- ^ [disp32]

data OperandRM s = OpReg (Reg s) | OpInd (Ind W64)

makePrisms ''OperandRM

class IsOperandRM op where
  toOperandRM :: op s -> OperandRM s

instance IsOperandRM Reg where
  toOperandRM = OpReg

-- | Return the base register of the indirect addressing operand, if there is one.
indBase :: Ind s -> Maybe (Reg s)
indBase (IndB base)           = Just base
indBase (IndBD8 base _)       = Just base
indBase (IndBD32 base _)      = Just base
indBase (IndBI base _ _)      = Just base
indBase (IndBID8 base _ _ _)  = Just base
indBase (IndBID32 base _ _ _) = Just base
indBase _                     = Nothing

-- | Return the index register of the indirect addressing operand, if there is one.
indIndex :: Ind s -> Maybe (Reg s)
indIndex (IndBI _ idx _)      = Just idx
indIndex (IndBID8 _ idx _ _)  = Just idx
indIndex (IndBID32 _ idx _ _) = Just idx
indIndex (IndID32 idx _ _)    = Just idx
indIndex _                    = Nothing

-- | REX byte needed for encoding indirect addressing operand.
indREX :: Ind s -> REX
indREX ind = foldMap (enableIf rexB . regIndexNeedsREX) (indBase ind)
  <>  foldMap (enableIf rexX . regIndexNeedsREX) (indIndex ind)
