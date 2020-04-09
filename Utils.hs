module Utils where

import Data.Bifunctor

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

toRight :: l -> Maybe r -> Either l r
toRight l = maybe (Left l) Right  

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f = bimap f id