{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module HJit.CodeGen.AMD64.Registers where

import HJit.CodeGen.AMD64.Types

-- * 64 Bit general purpose registers

rax :: GPReg 'Size64
rax = GPReg Size64 0x00

rcx :: GPReg 'Size64
rcx = GPReg Size64 0x01

rdx :: GPReg 'Size64
rdx = GPReg Size64 0x02

rbx :: GPReg 'Size64
rbx = GPReg Size64 0x03

rsp :: GPReg 'Size64
rsp = GPReg Size64 0x04

rbp :: GPReg 'Size64
rbp = GPReg Size64 0x05

rsi :: GPReg 'Size64
rsi = GPReg Size64 0x06

rdi :: GPReg 'Size64
rdi = GPReg Size64 0x07

r8  :: GPReg 'Size64
r8 = GPReg Size64 0x08

r9  :: GPReg 'Size64
r9 = GPReg Size64 0x09

r10 :: GPReg 'Size64
r10 = GPReg Size64 0x0A

r11 :: GPReg 'Size64
r11 = GPReg Size64 0x0B

r12 :: GPReg 'Size64
r12 = GPReg Size64 0x0C

r13 :: GPReg 'Size64
r13 = GPReg Size64 0x0D

r14 :: GPReg 'Size64
r14 = GPReg Size64 0x0E

r15 :: GPReg 'Size64
r15 = GPReg Size64 0x0F
