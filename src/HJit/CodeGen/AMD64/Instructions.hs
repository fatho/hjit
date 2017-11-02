{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
module HJit.CodeGen.AMD64.Instructions where

import           Data.Int
import           Data.Word

import           HJit.CodeGen.AMD64.Registers
import           HJit.CodeGen.AMD64.Types
import           HJit.CodeGen.Assembler

-- | Indirect addressing operand
data Ind (as :: WordSize) (vs :: WordSize)
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

-- | Operand that is either a register or a memory location
data RM s = RM_Reg (Reg s) | RM_Mem (Ind W64 s)

-- | Operand that is a relative jump target
data RelativeOffset
  = Rel8 Int8
  | Rel32 Int32

getOffset :: RelativeOffset -> Int32
getOffset (Rel8 rel) = fromIntegral rel
getOffset (Rel32 rel) = rel

data JumpTarget
  = JumpTargetRel RelativeOffset
  | JumpTargetLabel Label

data Instr
  = MovRegImm8 (Reg W8) Word8
  | MovRegImm16 (Reg W16) Word16
  | MovRegImm32 (Reg W32) Word32
  | MovRegImm64 (Reg W64) Word64

  | MovRmImm8 (RM W8) Word8
  | MovRmImm16 (RM W16) Word16
  | MovRmImm32 (RM W32) Word32
  | MovRmImm32SE (RM W64) Int32

  | Jmp JumpTarget
  | JmpReg (Reg W64)
  | Call JumpTarget
  | CallReg (Reg W64)
