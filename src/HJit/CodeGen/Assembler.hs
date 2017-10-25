module HJit.CodeGen.Assembler
  ( -- * Code generation monad
    Assembler
  , assemble
    -- * Code generation primitives
  , word8LE
  , word16LE
  , word32LE
  , word64LE
    -- * References
  , Label (..)
  , here
  ) where

import HJit.CodeGen.Assembler.Internal
