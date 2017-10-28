{-# LANGUAGE RecursiveDo #-}
module Main where

import Criterion.Main
import Control.Monad
import Data.Word

import HJit.CodeGen.AMD64

assertNoError :: Either e a -> a
assertNoError (Right x) = x
assertNoError (Left _) = error "assertion failed"

main :: IO ()
main = defaultMain
  [bgroup "assembler"
   [ bgroup "single instructions"
     [ bench "mov rax imm64" $ nf (assertNoError . assemble) (mov rax (0xdeadbeefdeadbeef :: Word64))
     , bench "mov ah imm8" $ nf (assertNoError . assemble) (mov ah (0xaf :: Word8))
     , bench "mov mem reg" $ nf (assertNoError . assemble) (mov (qwordPtr $ IndBID32 rsp r15 3 16) rax)
     ]
   ]
  ]
