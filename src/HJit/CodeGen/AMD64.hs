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

-- immediate to register

instance Movable (Reg W64) Word64 where
  mov reg val = emitOpReg (OpCodeByte 0xB8) reg >> word64LE val

instance Movable (Reg W32) Word32 where
  mov reg val = emitOpReg (OpCodeByte 0xB8) reg >> word32LE val

instance Movable (Reg W16) Word16 where
  mov reg val = emitOpReg (OpCodeByte 0xB8) reg >> word16LE val

instance Movable (Reg W8) Word8 where
  mov reg val = emitOpReg (OpCodeByte 0xB0) reg >> word8 val

instance Movable (Reg W64) Int64 where
  mov reg val = emitOpReg (OpCodeByte 0xB8) reg >> int64LE val

instance Movable (Reg W32) Int32 where
  mov reg val = emitOpReg (OpCodeByte 0xB8) reg >> int32LE val

instance Movable (Reg W16) Int16 where
  mov reg val = emitOpReg (OpCodeByte 0xB8) reg >> int16LE val

instance Movable (Reg W8) Int8 where
  mov reg val = emitOpReg (OpCodeByte 0xB0) reg >> int8 val

-- immediate to r/m

-- move with sign extension
instance Movable (Reg W64) Int32 where
  mov reg val = emitOpModRM (OpCodeByte 0xC7) (OpReg reg) (Reg 0 False) >> int32LE val

instance Movable (Ind W64) Word32 where
  -- use a fake 32 bit register with index 0 (required by opcode)
  mov ind val = emitOpModRM (OpCodeByte 0xC7) (OpInd ind) (Reg 0 False :: Reg W32) >> word32LE val

instance Movable (Ind W64) Word16 where
  mov ind val = emitOpModRM (OpCodeByte 0xC7) (OpInd ind) (Reg 0 False :: Reg W16) >> word16LE val

instance Movable (Ind W64) Word8 where
  mov ind val = emitOpModRM (OpCodeByte 0xC6) (OpInd ind) (Reg 0 False :: Reg W8) >> word8 val


-- r/m to register or register to r/m

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

-- TODO: implement MOV to/from segment register and (seg:offset) memory

-- TODO: implement MOVS (string operation)

-- * Stack manipulation

-- * Control flow

jmpReg :: Reg W64 -> AMD64 ()
jmpReg (Reg dst _) = do
  when (dst `testBit` 3) $
    emit rexB
  word8 0xFF
  emit $ mod11 <> modReg 0x4 <> modRM (dst .&. 0x7)

-- | return near (pop RIP from stack)
retn :: AMD64 ()
retn = word8 0xC3

-- * Prefixes

-- | Prefix certain opcodes to make them atomic.
lock :: AMD64 ()
lock = word8 0xF0

-- | Override opcode operand size. Ignored when REX is present, otherwise enables use of 16 bit operand.
overrideOperandSize :: AMD64 ()
overrideOperandSize = word8 0x66

-- | Override opcode address size to 32 bit.
overrideAddressSize :: AMD64 ()
overrideAddressSize = word8 0x67

-- * Helper functions

-- | Encode an opcode with a register operand that is encoded in the lowest 3 bits of the opcode.
emitOpReg :: WordSized s
          => OpCode   -- ^ base opcode (lowest 3 bit must be zero)
          -> Reg s    -- ^ register operand
          -> AMD64 ()
emitOpReg opcode reg@(Reg idx _) = do
  let size = wordSize reg
      -- for 64-bit operands, REX.W must be set
      rexSize = rexW `enableIf` (size == W64)
      -- set REX.B, if we use reg >= 8
      rexReg = rexB `enableIf` regIndexNeedsREX reg
      -- REX needed for encoding 8 bit special registers
      rex8 = rexNone `enableIf` (size == W8 && regNeedsLow8REX (unsafeCastReg reg))
      rexByte = rexSize <> rexReg <> rex8

  when (size == W16) $ overrideOperandSize
  when (isRexPresent rexByte) $ emit rexByte
  -- the lower 3 bits of the register are directly encoded in the opcode
  emit $ mapOpCode (+ idx .&. 7) opcode

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
      rex8 = rexNone `enableIf` (size == W8 && (regNeedsLow8REX (unsafeCastReg opreg) || rmNeedsLow8REX))
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

      -- emit ModRM byte to enable SIB addressing with the given mode
      useSIB mode = emit $ mode <> modOpReg <> modRM 0x4

      -- emit SIB byte for [base + disp8/32] (when mod = 01/10), [base] (when
      -- mod = 00 and base /= B.101), [disp32] (when mod = 00 and base = B.101)
      sibBaseDisp baseReg = sib 0 0x4 baseReg

      -- emit a generic SIB byte
      sib scale idx base = emit $ sibScale scale <> sibBase base <> sibIndex idx

  case oprm of
    OpReg rmreg -> emit $ mod11 <> modOpReg <> modRM (regIndex rmreg)
    OpInd ind -> case ind of
      IndB (Reg base _)
        -- R/M 0x4 in ModRM byte indicates SIB addressing, therefore this register needs to be encoded differently
        | base .&. 0x4 == 0x4 -> useSIB mod00 >> sibBaseDisp base                  -- SIB   [base]
        -- R/M 0x5 in ModRM byte indicates RIP-relative addressing, base
        -- 0x5 in SIP denotes [disp32]
        | base .&. 0x5 == 0x5 -> useSIB mod01 >> sibBaseDisp base >> word8 0       -- SIB   [base + 0x00]
        | otherwise -> emit $ mod00 <> modOpReg <> modRM base                      -- ModRM [base]
      IndBD8 (Reg base _) disp
        | base .&. 0x5 == 0x5 -> useSIB mod01 >> sibBaseDisp base >> int8 disp     -- SIB   [base + disp8]
        | otherwise -> emit (mod01 <> modOpReg <> modRM base) >> int8 disp         -- ModRM [base + disp8]
      IndBD32 (Reg base _) disp
        | base .&. 0x5 == 0x5 ->  useSIB mod10 >> sibBaseDisp base >> int32LE disp -- SIB   [base + disp32]
        | otherwise -> emit (mod10 <> modOpReg <> modRM base) >> int32LE disp      -- ModRM [base + disp32]

      IndBI (Reg base _) (Reg idx _) scale
        | idx == 0x4 {- RSP -} -> invalidInstruction "cannot use RSP as index"
        | base .&. 0x5 == 0x5 -> useSIB mod01 >> sib scale idx base >> word8 0     -- SIB   [base + index * 2^s + 0]
        | otherwise -> useSIB mod00 >> sib scale idx base                          -- SIB   [base + index * 2^s]
      IndBID8 (Reg base _) (Reg idx _) scale disp
        | idx == 0x4 {- RSP -} -> invalidInstruction "cannot use RSP as index"
        | otherwise -> useSIB mod01 >> sib scale idx base >> int8 disp             -- SIB   [base + index * 2^s + disp8]
      IndBID32 (Reg base _) (Reg idx _) scale disp
        | idx == 0x4 {- RSP -} -> invalidInstruction "cannot use RSP as index"
        | otherwise -> useSIB mod10 >> sib scale idx base >> int32LE disp          -- SIB   [base + index * 2^s + disp32]

      IndID32 (Reg idx _) scale disp
        | idx == 0x4 {- RSP -} -> invalidInstruction "cannot use RSP as index"
        | otherwise -> useSIB mod00 >> sib scale idx 0x5 >> int32LE disp          -- SIB   [index * 2^s + disp32]

      IndIP disp -> emit (mod00 <> modOpReg <> modRM 0x5) >> int32LE disp          -- ModRM [ip + disp32]
      IndD32 disp -> useSIB mod00 >> sibBaseDisp 0x5 >> int32LE disp               -- SIB   [disp32]
