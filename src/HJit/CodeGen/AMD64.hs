{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
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
module HJit.CodeGen.AMD64
  ( Movable (..)
  , retn
  , jmpReg
  -- * Primitives
  , lock
  , overrideOperandSize
  , overrideAddressSize
  -- * Convenience re-exports
  , module HJit.CodeGen.AMD64.Registers
  , module HJit.CodeGen.AMD64.Types
  , assemble
  , Label (..)
  , here
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Int
import           Data.Monoid
import           Data.Word

import           HJit.CodeGen.AMD64.Registers
import           HJit.CodeGen.AMD64.Types
import           HJit.CodeGen.Assembler
import           HJit.Util                    (enableIf)


-- * Move instructions

-- | Encodes a type safe way of moving data.
class Movable dst src where
  mov :: dst -> src -> AMD64 ()

instance Movable (Reg W64) Word64 where
  mov reg val = movRegImm 0xB8 rexW reg (word64LE val)

instance Movable (Reg W32) Word32 where
  mov reg val = movRegImm 0xB8 mempty reg (word32LE val)

instance Movable (Reg W16) Word16 where
  mov reg val = overrideOperandSize >> movRegImm 0xB8 mempty reg (word16LE val)

instance Movable (Reg W8) Word8 where
  mov reg val = movRegImm 0xB0 (rex `enableIf` regNeedsLow8REX reg) reg (word8LE val)


-- | Encode a move of an immediate operand into a register of the same size.
-- Encoding the necessary 0x66 operand size override and deciding whether a REX
-- prefix is needed for the given operand size is the responsibility of the
-- caller.
movRegImm :: Word8    -- ^ base opcode (decides 8 vs 16,32,64)
          -> REX      -- ^ REX prefix, if necessary
          -> Reg s    -- ^ register index
          -> AMD64 () -- ^ generating the operand
          -> AMD64 ()
movRegImm op rexBase reg@(Reg idx _) operand = do
  -- set REX.B, if we use reg >= 8
  let rexByte = rexBase <> (rexB `enableIf` regIndexNeedsREX reg)
  when (isRexPresent rexByte) $
    emit rexByte
  -- the lower 3 bits of the register are directly encoded in the immediate MOV opcode
  word8LE $ op + (idx .&. 7)
  operand

instance WordSized s => Movable (Reg s) (Reg s) where
  mov dst src
    | wordSize dst == W8 = emitOpModRM (OpCodeByte 0x88) (OpReg dst) src
    | otherwise          = emitOpModRM (OpCodeByte 0x89) (OpReg dst) src

instance WordSized s => Movable (Ind W64) (Reg s) where
  mov dst src
    | wordSize src == W8 = emitOpModRM (OpCodeByte 0x88) (OpInd dst) src
    | otherwise          = emitOpModRM (OpCodeByte 0x89) (OpInd dst) src

instance WordSized s => Movable (Reg s) (Ind W64) where
  mov dst src
    | wordSize dst == W8 = emitOpModRM (OpCodeByte 0x8A) (OpInd src) dst
    | otherwise          = emitOpModRM (OpCodeByte 0x8B) (OpInd src) dst

-- | Emit an opcode whose operands are encoded using a ModRM byte.
emitOpModRM :: WordSized s => OpCode -> OperandRM s -> Reg s -> AMD64 ()
emitOpModRM opcode oprm opreg = do
  let size = wordSize opreg
      -- for 64-bit operands, REX.W must be set
      rexSize = rexW `enableIf` (size == W64)
      -- REX needed for encoding the R/M operand
      rexRM = case oprm of
        OpReg rmreg -> rexB `enableIf` regIndexNeedsREX rmreg
        OpInd ind   -> indREX ind
      -- REX needed for encoding the Reg operand
      rexReg = rexR `enableIf` regIndexNeedsREX opreg
      -- REX needed for encoding 8 bit special registers
      rmNeedsLow8REX = anyOf _OpReg (regNeedsLow8REX . unsafeCastReg) oprm
      rex8 = rex `enableIf` (size == W8 && (regNeedsLow8REX (unsafeCastReg opreg) || rmNeedsLow8REX))
      -- check for collisions with AH, CH, DH or BH
      mustNotHaveREX = size == W8 && (regProhibitsREX (unsafeCastReg opreg)
                                      || anyOf _OpReg (regProhibitsREX . unsafeCastReg) oprm)

      -- combine REX
      rexByte = rexSize <> rexRM <> rexReg <> rex8

  when (mustNotHaveREX && isRexPresent rexByte) $ cannotEncodeWithRex

  -- for 16-bit operands, REX.W must not be set, and a 0x66 size override must be present
  when (size == W16) $ overrideOperandSize
  when (isRexPresent rexByte) $ emit rexByte
  emit opcode

  -- encode operands
  let modOpReg = modReg $ regIndex opreg
  case oprm of
    OpReg rmreg -> emit $ mod11 <> modOpReg <> modRM (regIndex rmreg)
    OpInd ind -> case ind of
      IndB base@(Reg breg _)
        -- R/M 0x4 in ModRM byte indicates SIB addressing, therefore this register needs to be encoded differently
        | breg .&. 0x4 == 0x4 -> do
            emit $ mod00 <> modOpReg <> modRM 0x4 -- use SIB addressing
            emit $ sibBase breg <> sibIndex 0x4 -- SIB [base]
        -- R/M 0x5 in ModRM byte indicates RIP-relative addressing, base 0x5 in SIP denotes [disp32]
        | breg .&. 0x5 == 0x5 -> do
            emit $ mod01 <> modOpReg <> modRM 0x4 -- use SIB addressing
            emit $ sibBase breg <> sibIndex 0x4 -- SIB [base + 0x00]
            word8LE 0
        | otherwise -> emit $ mod00 <> modOpReg <> modRM breg

  return ()


-- * Stack manipulation

-- * Control flow

jmpReg :: Reg W64 -> AMD64 ()
jmpReg (Reg dst _) = do
  when (dst `testBit` 3) $
    emit rexB
  word8LE 0xFF
  emit $ mod11 <> modReg 0x4 <> modRM (dst .&. 0x7)

-- | return near (pop RIP from stack)
retn :: AMD64 ()
retn = word8LE 0xC3

-- * Primitives

-- | Prefix certain opcodes to make them atomic.
lock :: AMD64 ()
lock = word8LE 0xF0

-- | Override opcode operand size. Ignored when REX is present, otherwise enables use of 16 bit operand.
overrideOperandSize :: AMD64 ()
overrideOperandSize = word8LE 0x66

-- | Override opcode address size to 32 bit.
overrideAddressSize :: AMD64 ()
overrideAddressSize = word8LE 0x67

-- * Helper functions

