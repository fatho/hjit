{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module HJit.CodeGen.AMD64 where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Int
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import           Data.Text              (Text)
import           Data.Word

import           HJit.CodeGen.Assembler

import           HJit.CodeGen.AMD64.Instructions

type Amd64 s = Asm s Instr

data Amd64Error = CannotHaveREXPrefix

