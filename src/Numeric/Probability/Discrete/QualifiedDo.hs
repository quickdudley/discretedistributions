module Numeric.Probability.Discrete.QualifiedDo where

import qualified Control.Monad(join)
import Numeric.Probability.Discrete

import qualified Prelude((>>),(>>=),(<$>),(<*>),return)
import Prelude hiding ((>>),(>>=),(<$>),(<*>),return)

return :: a -> Distribution a
return = Prelude.return

infixl 1 >>
(>>) :: Ord b => Distribution a -> Distribution b -> Distribution b
(>>) = flip const

infixl 1 >>=
(>>=) :: Ord b => Distribution a -> (a -> Distribution b) -> Distribution b
a >>= f = normalize $ a Prelude.>>= f

infixl 4 <$>
(<$>) :: (a -> b) -> Distribution a -> Distribution b
(<$>) = (Prelude.<$>)

infixl 4 <*>
(<*>) :: Distribution (a -> b) -> Distribution a -> Distribution b
(<*>) = (Prelude.<*>)

join :: Ord a => Distribution (Distribution a) -> Distribution a
join = normalize . Control.Monad.join

