-- | Module for all the common Haskell functions that have nothing to do with a compiler.
module Sara.Utils (foldlM, keyBy) where

import qualified Data.Map as M

-- | Applies a transformation of the environment that depends on a value for each value of a list.
foldlM :: Monad m => (a -> m (m b -> m b)) -> [a] -> m b -> m b
foldlM f xs m = foldl (\ a b -> (f b) >>= ($a)) m xs

-- | /O(n*log n)/. Build a map from a list of values and a function that computes the keys for the values.
-- The resulting map will for a particular keys have a list of all values for which the provided function returned the given key.
--
-- prop> keyBy f [] = empty
-- prop> keyBy f as `union` keyBy f bs = keyBy f (as ++ bs)
-- prop> keyBy (const k) as = singleton (k, as)
--
-- Example:
-- >> keyBy (mod 2) [1..4]
-- fromList [(0,[2,4]),(1,[1,3])]
keyBy :: Ord k => (a -> k) -> [a] -> M.Map k [a]
keyBy f = M.fromListWith (++) . map (\a -> (f a, [a]))
