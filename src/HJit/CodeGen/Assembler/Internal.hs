{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module HJit.CodeGen.Assembler.Internal
  ( -- * Code generation monad
    Asm (..)
  , AsmState (..)
  , runAsm
  , emit
    -- * References
  , Label (..)
  , SafeLabel (..)
  , here
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Hashable               (Hashable)
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq

-- * Monad for encoding instructions

data AsmState i = AsmState
  { _instructions :: !(Seq i)
    -- ^ code is encoded as a sequence of instructions
  }

makeLenses ''AsmState

-- | A label pointing at a specific instruction index (not a byte offset)
newtype Label = Label { labelPosition :: Int }
  deriving (Eq, Ord, Show, Hashable)

-- | A label pointing at a specific instruction index that cannot escaped the
-- assembler monad computation that generated it.
newtype SafeLabel s = SafeLabel { unsafeLabel :: Label }
  deriving (Eq, Ord, Show, Hashable)

newtype Asm s e i a = Asm { unwrapAsm :: StateT (AsmState i) (Either e) a }
  deriving (Functor, Applicative, Monad, MonadState (AsmState i), MonadError e, MonadFix)

runAsm :: (forall s. Asm s e i a) -> Either e (Seq i)
runAsm = over _Right (view instructions) . flip execStateT (AsmState Seq.empty) . unwrapAsm

-- | Emit a single instruction
emit :: i -> Asm s e i ()
emit instr = instructions %= (Seq.|> instr)

-- | Retrieve a label pointing to the current instruction index (where the next instruction will be added).
here :: Asm s e i (SafeLabel s)
here = SafeLabel . Label <$> uses instructions Seq.length
