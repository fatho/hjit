{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-| This module defines instructions that affect control flow. -}
module HJit.CodeGen.AMD64.ControlFlow
  ( Jumpable (..)
  , jmpLabel8
  , jmpLabel32
  , retn
  ) where

import           Control.Monad            (when)
import           Data.Bits                (Bits (..))
import           Data.Int
import           Data.Monoid              ((<>))

import           HJit.CodeGen.AMD64.Types
import           HJit.CodeGen.Assembler

class Jumpable a where
  jmp :: a -> AMD64 ()

instance Jumpable (Reg W64) where
  jmp (Reg dst _) = do
    when (dst `testBit` 3) $
      emit rexB
    word8 0xFF
    emit $ mod11 <> modReg 0x4 <> modRM (dst .&. 0x7)

instance Jumpable Int8 where
  jmp off = word8 0xEB >> int8 off

instance Jumpable Int32 where
  jmp off = word8 0xE9 >> int32LE off

withinReach :: forall a. (Integral a, Bounded a) => Int -> a
withinReach off
  | off >= fromIntegral (minBound :: a) && off <= fromIntegral (maxBound :: a) = fromIntegral off
  | otherwise = error "jump label to far away for chosen jump instruction"

-- | Jump to a label that is known to be reachable with a signed 8 bit offset.
-- If this condition is not fulfilled, 'error' will be called when the code is assembled.
jmpLabel8 :: Label -> AMD64 ()
jmpLabel8 target = mdo
  -- TODO: devise a mechanism for deferred errors depending on values from the future
  jmp (withinReach $ labelPosition target - labelPosition next :: Int8)
  next <- here
  return ()

-- | Jump to a label that is known to be reachable with a signed 8 bit offset.
-- If this condition is not fulfilled, 'error' will be called when the code is assembled.
jmpLabel32 :: Label -> AMD64 ()
jmpLabel32 target = mdo
  -- TODO: devise a mechanism for deferred errors depending on values from the future
  jmp (withinReach $ labelPosition target - labelPosition next :: Int32)
  next <- here
  return ()

-- | return near (pop RIP from stack)
retn :: AMD64 ()
retn = word8 0xC3
