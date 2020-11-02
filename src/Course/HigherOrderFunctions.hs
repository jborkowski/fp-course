-- |

module Course.HigherOrderFunctions where

import Prelude

-- Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using map and filter
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p =
  map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (h:t)
  | p h = h : takeWhile' p t
  | otherwise = []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> f a : acc) []

-- filter' :: (a -> b) -> [a] -> [b]
-- filter' p = foldr (\a acc -> if p a then a : acc else acc) []

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)


type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold ((==) 8 . length) (take 8) (drop 8)
