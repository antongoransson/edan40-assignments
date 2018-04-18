module Utilities where

-- Takes a pair (f1, f2) of functions and variables (a,b) and applies f1(a) f2(b)
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Applies a function on a maybe variable if the value is just
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- If first argument is Nothing pick second, else pick first
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

--The maybe function takes a default value (x), a function(id), and a Maybe value (f x). If the Maybe value is Nothing, the function returns the default value. Otherwise, it applies the function to the value inside the Just and returns the result.
-- Applies the function to x if x is Just else nothing
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Good use with substitute, will keep on going till no wildcards exists
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

--xs !! i works like xs[i]
-- Pick a index in lists  r  >= 0, r < 1 (probably used with random)
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
