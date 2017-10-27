module HJit.Util where

-- | Return the given monoid value if the condition is true, otherwise 'mempty'.
enableIf :: Monoid a => a -> Bool -> a
enableIf _ False = mempty
enableIf x True = x
