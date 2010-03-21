module Criterion.Utilities where

-- | Utility function to apply a function to a value n times.
applyntimes :: (a -> a) -> a -> Int -> a
applyntimes f start = (iterate f start!!)
