module Data.Extra.Ord where

-- | withinRange (b1, b2) a returns the closes value to a in the range [b1, b2]
withinRange :: Ord a => (a, a) -> a -> a
withinRange (a, b) x
 | x >= a && x <= b = x
 | x < a            = a
 | otherwise        = b
