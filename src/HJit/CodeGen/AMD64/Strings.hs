{-| Provides string based instructions. -}
module HJit.CodeGen.AMD64.Strings
  (
    -- * String move instructions
    movs, movsb, movsw, movsd, movsq
    -- * String load instructions
  , lods, lodsb, lodsw, lodsd, lodsq
    -- * String store instructions
  , stos, stosb, stosw, stosd, stosq
  ) where

import           HJit.CodeGen.AMD64.Prefixes
import           HJit.CodeGen.AMD64.Types
import           HJit.CodeGen.Assembler

-- * Move strings

-- | Move byte from [rsi] to [rdi]
movsb :: AMD64 ()
movsb = word8 0xA4

-- | Move word from [rsi] to [rdi]
movsw :: AMD64 ()
movsw = overrideOperandSize >> word8 0xA5

-- | Move dword from [rsi] to [rdi]
movsd :: AMD64 ()
movsd = word8 0xA5

-- | Move qword from [rsi] to [rdi]
movsq :: AMD64 ()
movsq = emit rexW >> word8 0xA5

-- | Move given number of bytes from [rsi] to [rdi]
movs :: WordSize -> AMD64 ()
movs W8  = movsb
movs W16 = movsw
movs W32 = movsd
movs W64 = movsq

-- * Load strings

-- | Load byte from [rsi] to al
lodsb :: AMD64 ()
lodsb = word8 0xAC

-- | Load word from [rsi] to ax
lodsw :: AMD64 ()
lodsw = overrideOperandSize >> word8 0xAD

-- | Load dword from [rsi] to eax
lodsd :: AMD64 ()
lodsd = word8 0xAD

-- | Load qword from [rsi] to rax
lodsq :: AMD64 ()
lodsq = emit rexW >> word8 0xAD

-- | Load given number of bytes from [rsi] to al, ax, eax, rax
lods :: WordSize -> AMD64 ()
lods W8  = lodsb
lods W16 = lodsw
lods W32 = lodsd
lods W64 = lodsq

-- * Store strings

-- | store byte from al to [rdi]
stosb :: AMD64 ()
stosb = word8 0xAC

-- | store word from ax to [rdi]
stosw :: AMD64 ()
stosw = overrideOperandSize >> word8 0xAD

-- | store dword from eax to [rdi]
stosd :: AMD64 ()
stosd = word8 0xAD

-- | store qword from rax to [rdi]
stosq :: AMD64 ()
stosq = emit rexW >> word8 0xAD

-- | store given number of bytes from al, ax, eax, rax to [rdi]
stos :: WordSize -> AMD64 ()
stos W8  = stosb
stos W16 = stosw
stos W32 = stosd
stos W64 = stosq

-- TODO: implement SCAS, CMPS
