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
module HJit.CodeGen.AMD64
  ( Movable (..)
  , retn
  , jmpReg
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

-- * Move instructions

-- | Encodes a type safe way of moving data.
class Movable dst src where
  mov :: dst -> src -> AMD64 ()

instance Movable (GPReg 'Size64) Word64 where
  mov (GPReg _ reg) val = do
    -- W to have a 64-bit operand
    -- B, if we use reg >= 8
    opcode $ rexW <> REX ((reg .&. 8) `shiftR` 3)
    -- the lower 3 bits of the register are directly encoded in the 0xB8+r MOV opcode
    word8LE $ 0xB8 + (reg .&. 7)
    word64LE val

instance Movable (GPReg 'Size64) (GPReg 'Size64) where
  mov (GPReg _ dst) (GPReg _ src) = do
    -- W to have a 64-bit operand
    -- R, if we use src >= 8
    -- B, if we use dst >= 8
    opcode $ rexW <> REX ((dst .&. 8) `shiftR` 3) <> REX ((src .&. 8) `shiftR` 1)
    word8LE 0x89
    -- Mod 11, REG src, RM dst
    -- the lower 3 bits of the register are directly encoded in the 0xB8+r MOV opcode
    opcode $ mod11 <> modReg (src .&. 0x7) <> modRM (dst .&. 0x7)

-- * Stack manipulation

-- * Control flow

jmpReg :: GPReg 'Size64 -> AMD64 ()
jmpReg (GPReg _ dst) = do
  when (testBit dst 3) $
    opcode $ rexB
  word8LE 0xFF
  opcode $ mod11 <> modReg 0x4 <> modReg (dst .&. 0x7)

-- | return near (pop RIP from stack)
retn :: AMD64 ()
retn = word8LE 0xC3
