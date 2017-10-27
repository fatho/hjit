{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module HJit.CodeGen.AMD64.Registers where

import           Data.Maybe               (fromJust)

import           HJit.CodeGen.AMD64.Types

-- * 64 Bit general purpose registers

rax :: Reg S64
rax = Reg 0x00 False

rcx :: Reg S64
rcx = Reg 0x01 False

rdx :: Reg S64
rdx = Reg 0x02 False

rbx :: Reg S64
rbx = Reg 0x03 False

rsp :: Reg S64
rsp = Reg 0x04 False

rbp :: Reg S64
rbp = Reg 0x05 False

rsi :: Reg S64
rsi = Reg 0x06 False

rdi :: Reg S64
rdi = Reg 0x07 False

r8  :: Reg S64
r8 = Reg 0x08 False

r9  :: Reg S64
r9 = Reg 0x09 False

r10 :: Reg S64
r10 = Reg 0x0A False

r11 :: Reg S64
r11 = Reg 0x0B False

r12 :: Reg S64
r12 = Reg 0x0C False

r13 :: Reg S64
r13 = Reg 0x0D False

r14 :: Reg S64
r14 = Reg 0x0E False

r15 :: Reg S64
r15 = Reg 0x0F False


-- | Return the corresponding register of a different size. For truncating
-- casts, this always returns the register corresponding to lowest n bits of the
-- 64 bit variant.
castRegSize :: forall src dst. (ReifySize src, ReifySize dst) => Reg src -> Reg dst
castRegSize (Reg reg _) = Reg reg False

-- | Return the register corresponding to the high 8 bits of the lower word, if accessible.
castRegSize8H :: Reg src -> Maybe (Reg S8)
castRegSize8H (Reg reg _)
  | reg < 4 = Just (Reg (reg + 4) True)
  | otherwise = Nothing

-- * 32 bit general purpose registers

eax :: Reg S32
eax = castRegSize rax

r8d :: Reg S32
r8d = castRegSize r8

-- TODO: remaining 32 bit registers

-- * 16 bit general purpose registers

ax :: Reg S16
ax = castRegSize rax

r8w :: Reg S16
r8w = castRegSize r8

-- TODO: remaining 16 bit registers

-- * 8 bit general purpose registers

al :: Reg S8
al = castRegSize rax

ah :: Reg S8
ah = fromJust $ castRegSize8H rax

spl :: Reg S8
spl = castRegSize rsp

r8l :: Reg S8
r8l = castRegSize r8

-- TODO: remaining 8 bit registers
