module Numeric.Probability.Discrete (
  Distribution,
  runDistribution,
  proportional,
  uniform,
  normalize,
  normalize',
  observe,
  determine,
  probabilityOf
 ) where

import Data.List
import Data.Function (on)
import Data.Ratio ((%))

-- | Represents a discrete probability distribution
newtype Distribution a = P {runDistribution :: [(Rational,a)]}

instance Functor Distribution where
  fmap f (P a) = P $ (fmap . fmap) f a

instance Applicative Distribution where
  pure a = P [(1,a)]
  P f <*> P b = P [(pf * pb, f' b') | ~(pf,f') <- f, ~(pb,b') <- b]

instance Monad Distribution where
  return = pure
  P a >>= f = P [(pa * pb,b) | ~(pa,a') <- a, ~(pb,b) <- runDistribution (f a')]

instance Show a => Show (Distribution a) where
  showsPrec _ (P d) = ("proportional "++) . showsPrec 11 d

instance (Ord n, Num n) => Num (Distribution n) where
  a + b = normalize $ (+) <$> a <*> b
  a * b = normalize $ (*) <$> a <*> b
  a - b = normalize $ (-) <$> a <*> b
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

-- | Creates a distribution with the probability of each outcome being
-- proportional to the given weight.
proportional :: Real p => [(p,a)] -> Distribution a
proportional [] = error "proportional: Probability distribution cannot be empty"
proportional l' = let
  l = map (\ ~(p,v) -> (toRational p, v)) $ filter ((> 0) . fst) l'
  t = sum $ map fst l
  in P $ map (\ ~(p,v) -> (p / t, v)) l

-- | Creates a uniform probability distribution
uniform :: [a] -> Distribution a
uniform [] = error "uniform: Probability distribution cannot be empty"
uniform l = let
  p = 1 % (genericLength l)
  in P $ map ((,) p) l

-- | Merge duplicated outcomes.
normalize :: Ord a => Distribution a -> Distribution a
normalize (P l) = P $ map (\g@(~(~(_,v):_)) -> (sum $ map fst g, v)) $
  groupBy ((==) `on` snd) $
  sortBy (compare `on` snd) l

-- | Merge duplicated outcomes (slower but does not require 'Ord' instance)
normalize' :: Eq a => Distribution a -> Distribution a
normalize' = P . go . runDistribution where
  go [] = []
  go ((p1,a):r) = let
    (h,z) = partition ((== a) . snd) r
    in (p1 + sum (map fst h), a) : go z

-- | Bayesian inference (or fuzzy 'determine')
observe :: (a -> Distribution Bool) -> Distribution a -> Distribution a
observe p (P l) = proportional $ do
  ~(p1,a) <- l
  ~(p2,i) <- runDistribution $ p a
  if i then [(p1 * p2, a)] else []

-- | Remove outcomes which do not satisfy the predicate. Similar to 'filter'
determine :: (a -> Bool) -> Distribution a -> Distribution a
determine p (P l) = proportional $ filter (p . snd) l

-- | Calculates the probability of the predicate being satisfied
probabilityOf :: (a -> Bool) -> Distribution a -> Rational
probabilityOf p (P l) = sum $ map fst $ filter (p . snd) l