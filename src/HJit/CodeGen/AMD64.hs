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

import           Control.Monad
import           Data.Bits
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

instance Movable (Reg S64) Word64 where
  mov reg val = movRegImm 0xB8 rexW reg (word64LE val)

instance Movable (Reg S32) Word32 where
  mov reg val = movRegImm 0xB8 mempty reg (word32LE val)

instance Movable (Reg S16) Word16 where
  mov reg val = overrideOperandSize >> movRegImm 0xB8 mempty reg (word16LE val)

instance Movable (Reg S8) Word8 where
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
    opcode rexByte
  -- the lower 3 bits of the register are directly encoded in the immediate MOV opcode
  word8LE $ op + (idx .&. 7)
  operand

instance Movable (Reg S64) (Reg S64) where
  mov dst src = movRegReg 0x89 rexW dst src

instance Movable (Reg S32) (Reg S32) where
  mov dst src = movRegReg 0x89 rexW dst src

instance Movable (Reg S16) (Reg S16) where
  mov dst src = overrideOperandSize >> movRegReg 0x89 mempty dst src

instance Movable (Reg S8) (Reg S8) where
  mov dst src
    | any regProhibitsREX [src, dst] && any regNeedsREX [src, dst] = cannotEncodeWithRex
    | otherwise = movRegReg 0x88 (rex `enableIf` any regNeedsLow8REX [src, dst]) dst src
    where
      regNeedsREX r = regNeedsLow8REX r || regIndexNeedsREX r

-- | Encode a move from a register to a register of the same size.
-- TODO: generalize for arbitrary Reg RM moves
movRegReg :: Word8 -- ^ move opcode (decides rm -> reg vs reg -> rm)
          -> REX   -- ^ REX prefix, if necessary
          -> Reg s -- ^ destination register
          -> Reg s -- ^ source register
          -> AMD64 ()
movRegReg op rexBase (Reg dst _) (Reg src _) = do
  -- R, if we use src >= 8
  -- B, if we use dst >= 8
  let rexByte = rexBase <> (rexB `enableIf` (testBit dst 3)) <> (rexR `enableIf` (testBit src 3))
  when (isRexPresent rexByte) $
    opcode rexByte
  word8LE op
  -- Mod 11, REG src, RM dst
  -- the lower 3 bits of the register are directly encoded in the 0xB8+r MOV opcode
  opcode $ mod11 <> modReg (src .&. 0x7) <> modRM (dst .&. 0x7)

-- * Stack manipulation

-- * Control flow

jmpReg :: Reg S64 -> AMD64 ()
jmpReg (Reg dst _) = do
  when (dst `testBit` 3) $
    opcode rexB
  word8LE 0xFF
  opcode $ mod11 <> modReg 0x4 <> modReg (dst .&. 0x7)

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

-- | Check whether the register index needs a REX prefix for encoding.
regIndexNeedsREX :: Reg s -> Bool
regIndexNeedsREX (Reg reg _) = testBit reg 3

-- | Check whether a REX prefix is needed for selecting the correct low 8 bit register.
regNeedsLow8REX :: Reg S8 -> Bool
regNeedsLow8REX (Reg idx False) = idx >= 4 && idx <= 7
regNeedsLow8REX _ = False

-- | Check whether a REX prefix prevents encoding the given register.
regProhibitsREX :: Reg S8 -> Bool
regProhibitsREX (Reg idx True) = idx >= 4 && idx <= 7
regProhibitsREX _ = False
