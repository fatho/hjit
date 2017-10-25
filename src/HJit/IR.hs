{-# LANGUAGE GADTs #-}
{-| This modules defines the intermediate representation (IR) for the code to be
  generated. -}
module HJit.IR where

newtype Code a = Code { runCode :: a }

newtype Node a = Node { nodeId :: Int }

class Expr repr where
  ival :: Int -> repr Int

expr :: Expr e => e a -> Code ()
expr = undefined

--fun :: (Node a -> Code a) -> 

-- foo = fun $ \x -> do
--   z <- expr $ x * 3
--   ret z
