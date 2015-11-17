-- | Helper functions to generate values of primitives.
-- These are copied from Test.QuickCheck.Arbitrary and adjusted to use MonadGen
module Sara.TestUtils.ArbitraryUtils where

import Sara.TestUtils.GenT
import Data.Ratio ( (%) )

-- | Generates a boolean.
arbitraryBool :: MonadGen g => g Bool
arbitraryBool = choose (False, True)

-- | Generates a natural number. The number's maximum value depends on
-- the size parameter.
arbitrarySizedNatural :: (Integral a, MonadGen g) => g a
arbitrarySizedNatural =
  sized $ \n ->
  inBounds fromInteger (choose (0, toInteger n))

-- | Generates an integral number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedIntegral :: (Integral a, MonadGen g) => g a
arbitrarySizedIntegral =
  sized $ \n ->
  inBounds fromInteger (choose (-toInteger n, toInteger n))

inBounds :: (Integral a, MonadGen g) => (Integer -> a) -> g Integer -> g a
inBounds fi g = fmap fi (g `suchThat` (\x -> toInteger (fi x) == x))

-- | Generates a fractional number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedFractional :: (Fractional a, MonadGen g) => g a
arbitrarySizedFractional =
  sized $ \n ->
    let n' = toInteger n in
      do a <- choose ((-n') * precision, n' * precision)
         b <- choose (1, precision)
         return (fromRational (a % b))
 where
  precision = 9999999999999 :: Integer
