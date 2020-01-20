module Unique(
  WithUnique,
  unique,
  withUniques
) where

import Control.Monad
import Data.List
import Data.Int
import Data.Eq
-- import Data.List.Utils (replace)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y (a : as) = if a == x then y : as else a : as

-- data WithUnique a = WithUnique Int a
--
-- instance Functor WithUnique where
--   fmap f (WithUnique n a) = WithUnique n (f a)
--
-- instance Applicative WithUnique where
--   pure = WithUnique 0
--   (WithUnique n f) <*> (WithUnique m a) = WithUnique (max n m) (f a)
--
-- instance Monad WithUnique where
--   return = WithUnique 0
--   (WithUnique n a) >>= f = let (WithUnique m b) = f a
--                            in WithUnique (max m n) b

newtype WithUnique u a = WithUnique {withUniques :: [u] -> (a,[u]) }

instance Functor (WithUnique u) where
  fmap f (WithUnique utoa) = WithUnique
    (\us -> let (a, us') = utoa us in (f a, us'))

instance Applicative (WithUnique u) where
  pure a = WithUnique (\u -> (a, u))
  (WithUnique utof) <*> (WithUnique utoa) =
    -- WithUnique (\us -> let (a, us') = utoa us
                           -- (f, us'') = utof us'
                       -- in  (f a, us''))
    WithUnique (\us -> let (f, us') = utof us
                           (a, us'') = utoa us'
                       in  (f a, us''))


instance Monad (WithUnique u) where
  return a = WithUnique (\u -> (a, u))
  (WithUnique utoa) >>= f = WithUnique (\us ->
      let (a, us') = utoa us
          (WithUnique fa) = f a
      in fa us')

uniqueImpl :: [u] -> (u, [u])
uniqueImpl (u : us) = (u, us)

unique :: WithUnique u u
unique = WithUnique uniqueImpl

merge :: (Eq u) => u -> u -> WithUnique u ()
merge u1 u2 = WithUnique (\us -> ((), replace u1 u2 us))

-- runWithUnique : (WithUnique u a) -> [u] -> a
-- runWithUnique wu = let (a us) = withUniques in a

example :: WithUnique Char [Char]
example = do
  a <- unique
  b <- unique
  merge a b
  return ['h', a, a, a, b]
