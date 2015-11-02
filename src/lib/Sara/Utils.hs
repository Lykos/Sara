-- | Module for all the common Haskell functions that have nothing to do with a compiler.
module Sara.Utils (foldlM) where

import Control.Monad

-- TODO Improve! Nobody understands this, not even yourself! And it is ugly! Probably it is possible to do a nice version.
foldlM :: Monad m => (a -> m (m b -> m b)) -> [a] -> m b -> m b
foldlM f xs m = join $ foldM f' m xs
  where f' m x = do
          f'' <- f x
          return $ f'' m
