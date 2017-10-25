{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
module HJit.CodeGen.Assembler.Internal
  ( -- * Code generation monad
    Assembler (..)
  , AssemblerState (..)
  , codeSize
  , code
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

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import           Data.Word                  (Word16, Word32, Word64, Word8)

-- * Monad for encoding instructions

-- | Internal state of the assembler
data AssemblerState = AssemblerState
  { _codeSize :: !Int
  -- ^ the size of the code
  , _code     :: !BB.Builder
  -- ^ the generated code
  }
makeLenses ''AssemblerState

-- | A label marking a specific position in the generated code.
data Label = Label { labelPosition :: Int }
-- NOTE: the label type is intentionally `data`, not `newtype` in order to
-- enforce laziness for the stored position. This prevents accidental
-- non-termination when using MonadFix features.

newtype Assembler e a = Assembler { runAssembler :: StateT AssemblerState (Either e) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadError e, MonadState AssemblerState)

assemble :: Assembler e a -> Either e BL.ByteString
assemble = fmap (BB.toLazyByteString . _code) . flip execStateT (AssemblerState 0 mempty) . runAssembler

word8LE :: Word8 -> Assembler e ()
word8LE x = do
  code <>= BB.word8 x
  codeSize += 1

word16LE :: Word16 -> Assembler e ()
word16LE x = do
  code <>= BB.word16LE x
  codeSize += 2

word32LE :: Word32 -> Assembler e ()
word32LE x = do
  code <>= BB.word32LE x
  codeSize += 4

word64LE :: Word64 -> Assembler e ()
word64LE x = do
  code <>= BB.word64LE x
  codeSize += 8

here :: Assembler e Label
here = Label <$> use codeSize

