module HJit.CodeGen.Assembler
  ( -- * Code generation monad
    Asm
  , runAsm
  , emit
    -- * References
  , Label (..)
  , here
  ) where

import HJit.CodeGen.Assembler.Internal
