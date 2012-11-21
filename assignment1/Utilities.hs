module Utilities where

-- Higher order function, returns a tuple where f1 is called on x1, and f2 called on x2
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Higher order function, pass in a function that transforms a value a->b and an arg a and return a maybe of the arg b
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- returns first argument if it exists, does not have the value Nothing, otherwise it returns the second argument
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- try to apply a function that returns a Maybe on the arg x. If the function call results in Nothing x is returned instead.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- apply the function f on the arg x until the function does not modify the state anymore 
-- Maybe it could be used in longerWildcardMatch to step the matchlist to the same char as the one after the wildcard in the pattern.
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

