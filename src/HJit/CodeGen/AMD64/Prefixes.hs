{-| This module defines opcode prefixes. -}
module HJit.CodeGen.AMD64.Prefixes
  ( lock
  , overrideOperandSize
  , overrideAddressSize
  , twoByteOpCode
  ) where

import HJit.CodeGen.AMD64.Types
import HJit.CodeGen.Assembler

-- | Emit the two-byte opcode prefix
twoByteOpCode :: AMD64 ()
twoByteOpCode = word8 0x0F

-- | Prefix certain opcodes to make them atomic.
lock :: AMD64 ()
lock = word8 0xF0

-- | Override opcode operand size. Ignored when REX is present, otherwise enables use of 16 bit operand.
overrideOperandSize :: AMD64 ()
overrideOperandSize = word8 0x66

-- | Override opcode address size to 32 bit.
overrideAddressSize :: AMD64 ()
overrideAddressSize = word8 0x67
