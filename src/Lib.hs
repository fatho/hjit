{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecursiveDo          #-}
module Lib
    ( someFunc
    ) where

import Data.Foldable
import Data.Monoid
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Text.Printf (printf)

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import HJit.Memory

import HJit.CodeGen.AMD64

type TestFunction = IO CLLong
foreign import ccall "dynamic"
  mkFun :: FunPtr TestFunction -> TestFunction

someFunc :: IO ()
someFunc = do
  let memsize = 4096
  memx <- allocateRegion memsize (accessRead <> accessWrite <> accessExec)
  let ptr = regionPtr (memx :: Region Word8)
      code = BL.unpack $ either (error . show) id $ assemble (testAsm $ fromIntegral $ ptrToIntPtr ptr)
  for_ (zip [0..] code) $ \(idx, b) -> do
    printf "%02x " b
    pokeByteOff ptr idx b
  printf "\n"
  print memx
  -- TODO: generate code here
  protectRegion (accessRead <> accessWrite) memx
  let fun = castPtrToFunPtr ptr :: FunPtr TestFunction
  x <- mkFun fun
  printf "%016x" (fromIntegral x :: Word64)
  -- TODO: execute code here
  freeRegion memx


testAsm :: Int -> AMD64 ()
testAsm base = mdo
  mov r15 (0xdeadbeefdeafacdc :: Word64)
  mov rax (fromIntegral $ labelPosition skip + base :: Word64)
  jmpReg rax
  mov r15 (2 :: Word64)
  skip <- here
  mov rax r15
  mov eax (0xabbafefe :: Word32)
  mov ax (0xabcd :: Word16)
  mov al (0x34 :: Word8)
  mov ah (0x12 :: Word8)
  retn
