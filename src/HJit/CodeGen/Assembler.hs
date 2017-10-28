module HJit.CodeGen.Assembler
  ( -- * Code generation monad
    Assembler
  , assemble
    -- * Code generation primitives
  , int8
  , int16LE
  , int32LE
  , int64LE
  , word8
  , word16LE
  , word32LE
  , word64LE
    -- * References
  , Label (..)
  , here
  ) where

import HJit.CodeGen.Assembler.Internal
