module HJit.Util where

import Data.Proxy

-- | Return the given monoid value if the condition is true, otherwise 'mempty'.
enableIf :: Monoid a => a -> Bool -> a
enableIf _ False = mempty
enableIf x True = x


canRepresent :: (Integral a, Integral b) => a -> Proxy b -> Bool
canRepresent a proxy =
  let b = fromIntegral a `asTypeOfProxy` proxy
      aAgain = fromIntegral b `asTypeOf` a
  in a == aAgain


asTypeOfProxy :: a -> Proxy a -> a
asTypeOfProxy x _ = x
