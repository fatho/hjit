{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-| This module defines the various MOV instructions in a uniform way. -}
module HJit.CodeGen.AMD64.Moves
  ( -- * General move instructions
    Movable (..)
  ) where

import           Data.Int
import           Data.Word

import           HJit.CodeGen.AMD64.Primitives
import           HJit.CodeGen.AMD64.Types
import           HJit.CodeGen.Assembler

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

instance Movable (Ind W64 W32) Word32 where
  -- use a fake register with index 0 (required by opcode)
  mov ind val = emitOpModRM (OpCodeByte 0xC7) (OpInd ind) (Reg 0 False) >> word32LE val

instance Movable (Ind W64 W16) Word16 where
  mov ind val = emitOpModRM (OpCodeByte 0xC7) (OpInd ind) (Reg 0 False) >> word16LE val

instance Movable (Ind W64 W8) Word8 where
  mov ind val = emitOpModRM (OpCodeByte 0xC6) (OpInd ind) (Reg 0 False) >> word8 val


-- r/m to register or register to r/m

instance WordSized s => Movable (Reg s) (Reg s) where
  mov dst src
    | wordSize dst == W8 = emitOpModRM (OpCodeByte 0x88) (OpReg dst) src
    | otherwise          = emitOpModRM (OpCodeByte 0x89) (OpReg dst) src

instance WordSized s => Movable (Ind W64 s) (Reg s) where
  mov dst src
    | wordSize src == W8 = emitOpModRM (OpCodeByte 0x88) (OpInd dst) src
    | otherwise          = emitOpModRM (OpCodeByte 0x89) (OpInd dst) src

instance WordSized s => Movable (Reg s) (Ind W64 s) where
  mov dst src
    | wordSize dst == W8 = emitOpModRM (OpCodeByte 0x8A) (OpInd src) dst
    | otherwise          = emitOpModRM (OpCodeByte 0x8B) (OpInd src) dst

-- TODO: implement MOV to/from segment register and (seg:offset) memory

-- TODO: implement MOVSX (sign extension)

