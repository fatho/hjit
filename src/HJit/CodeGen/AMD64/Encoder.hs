{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module HJit.CodeGen.AMD64.Encoder where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits                       (Bits (..))
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Int
import           Data.Monoid                     ((<>))
import           Data.Proxy
import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq
import           Data.Text                       (Text)
import           Data.Word

import           HJit.CodeGen.AMD64.Instructions
import           HJit.CodeGen.AMD64.Registers
import           HJit.CodeGen.AMD64.Types
import           HJit.CodeGen.Assembler
import           HJit.Util

-- | Monad that interprets 'MonadEncode' computations by computing the size of the generated instructions.
newtype ComputeInstructionSize a = ComputeInstructionSize { runComputeInstructionSize :: State Word a }
  deriving (Functor, Applicative, Monad, MonadState Word)

class Monad m => MonadEncode m where
  word8 :: Word8 -> m ()
  word16LE :: Word16 -> m ()
  word32LE :: Word32 -> m ()
  word64LE :: Word64 -> m ()
  int8 :: Int8 -> m ()
  int16LE :: Int16 -> m ()
  int32LE :: Int32 -> m ()
  int64LE :: Int64 -> m ()

instance MonadEncode ComputeInstructionSize where
  word8 _ = id += 1
  word16LE _ = id += 2
  word32LE _ = id += 4
  word64LE _ = id += 8
  int8 _ = id += 1
  int16LE _ = id += 2
  int32LE _ = id += 4
  int64LE _ = id += 8

class Encodable a where
  encode :: MonadEncode m => a -> m ()

-- | Representation of AMD64 opcodes
data Opcode
  = Opcode1 Word8
  -- ^ 1-byte opcode
  | Opcode2 Word8
  -- ^ 2-byte opcode, prefixed with 0x0F

opcodeByte :: Lens' Opcode Word8
opcodeByte = lens g s where
  g (Opcode1 x) = x
  g (Opcode2 x) = x
  s (Opcode1 _) = Opcode1
  s (Opcode2 _) = Opcode2

instance Encodable Opcode where
  encode (Opcode1 code) = word8 code
  encode (Opcode2 code) = word8 0x0F >> word8 code

-- | Optional REX prefix byte. This type encodes the invariant that it is either 0 or the highest 4 bits are always 0x4.
newtype REX = REX { getRex :: Word8 }

instance Monoid REX where
  mempty = REX 0x00
  mappend (REX a) (REX b) = REX (a .|. b)

instance Encodable REX where
  encode = word8 . getRex

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

instance Encodable ModRM where
  encode = word8 . getModRM

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

-- | SIB byte (scale, index, base)
newtype SIB = SIB { getSIB :: Word8 }

instance Monoid SIB where
  mempty = SIB 0
  mappend (SIB a) (SIB b) = SIB (a .|. b)

instance Encodable SIB where
  encode = word8 . getSIB

sibScale :: Scale -> SIB
sibScale s = SIB $ (s .&. 0x3) `shiftL` 6

sibIndex :: Word8 -> SIB
sibIndex i = SIB $ (i .&. 0x7) `shiftL` 3

sibBase :: Word8 -> SIB
sibBase b = SIB $ (b .&. 0x7)

-- * Prefixes

-- | General type for prefix bytes that modify instructions
data Prefix = Prefix8 Word8

instance Encodable Prefix where
  encode (Prefix8 prefix) = word8 prefix

-- | Prefix certain opcodes to make them atomic.
lock :: Prefix
lock = Prefix8 0xF0

-- | Override opcode operand size. Ignored when REX is present, otherwise enables use of 16 bit operand.
overrideOperandSize :: Prefix
overrideOperandSize = Prefix8 0x66

-- | Override opcode address size to 32 bit.
overrideAddressSize :: Prefix
overrideAddressSize = Prefix8 0x67


-- * Encoding instructions

-- | Maps labels to byte offsets.
type LabelOffsets = HashMap Label Int

encodeInstruction :: MonadEncode m => LabelOffsets -> Instr -> m ()
encodeInstruction labels = go where
  go (MovRegImm8 reg imm) = emitOpReg (Opcode1 0xB0) reg >> word8 imm
  go (MovRegImm16 reg imm) = emitOpReg (Opcode1 0xB8) reg >> word16LE imm
  go (MovRegImm32 reg imm) = emitOpReg (Opcode1 0xB8) reg >> word32LE imm
  go (MovRegImm64 reg imm) = emitOpReg (Opcode1 0xB8) reg >> word64LE imm

  go (Jmp tgt) = case jmpToRel tgt of
    Rel8 rel -> word8 0xEB >> int8 rel
    Rel32 rel -> word8 0xE9 >> int32LE rel
  go (JmpReg (Reg dst _)) = do
    when (dst `testBit` 3) $
      encode rexB
    word8 0xFF
    encode $ mod11 <> modReg 0x4 <> modRM (dst .&. 0x7)

  go (Call tgt) = word8 0xE8 >> int32LE (getOffset $ jmpToRel tgt)
  go (CallReg (Reg dst _)) = do
    when (dst `testBit` 3) $
      encode rexB
    word8 0xFF
    encode $ mod11 <> modReg 0x2 <> modRM (dst .&. 0x7)

  jmpToRel (JumpTargetRel rel) = rel
  jmpToRel (JumpTargetLabel lbl) = resolveLabel lbl

  resolveLabel lbl = case HashMap.lookup lbl labels of
    Nothing -> error "cannot happen"
    Just off | canRepresent off (Proxy :: Proxy Int8) -> Rel8 $ fromIntegral off
             | canRepresent off (Proxy :: Proxy Int32) -> Rel32 $ fromIntegral off
             | otherwise -> error "label too far away, not implemented"

instance Encodable RelativeOffset where
  encode (Rel8 rel) = int8 rel
  encode (Rel32 rel) = int32LE rel

-- | Encode an opcode with a register operand that is encoded in the lowest 3 bits of the opcode.
emitOpReg :: (MonadEncode m, WordSized s)
          => Opcode   -- ^ base opcode (lowest 3 bit must be zero)
          -> Reg s   -- ^ register operand
          -> m ()
{-# INLINABLE emitOpReg #-}
emitOpReg opcode reg@(Reg idx _) = do
  let size = wordSizeOf reg
      -- for 64-bit operands, REX.W must be set
      rexSize = rexW `enableIf` (size == W64)
      -- set REX.B, if we use reg >= 8
      rexReg = rexB `enableIf` regIndexNeedsREX reg
      -- REX needed for encoding 8 bit special registers
      rex8 = rexNone `enableIf` (size == W8 && regNeedsLow8REX (unsafeCastReg reg))
      rexByte = rexSize <> rexReg <> rex8

  when (size == W16) $ encode overrideOperandSize
  when (isRexPresent rexByte) $ encode rexByte
  -- the lower 3 bits of the register are directly encoded in the opcode
  encode (opcode & opcodeByte +~ idx .&. 7)

