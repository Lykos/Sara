-- | Module for all the common Haskell functions that have nothing to do with a compiler.
module Sara.Utils (foldlM) where

-- TODO Improve! Nobody understands this, not even yourself! And it is ugly! Probably it is possible to do a nice version.
-- Try to at least understand what the variables mean and give them appropriate names.
foldlM :: Monad m => (a -> m (m b -> m b)) -> [a] -> m b -> m b
foldlM f xs m = foldl (\ a b -> (f b) >>= ($a)) m xs
