module Main where

myProduct :: Num a => [a] -> a
myProduct [] = 0;
myProduct (x:xs) = x + myProduct xs;

myConcat :: [[a]] -> [a]
myConcat xs = myConcat' [] xs where
  myConcat' acc [] = acc;
  myConcat' acc (x:xs) = myConcat' (acc ++ x) xs;

myAnd :: [Bool] -> Bool
myAnd (x:xs) = myAnd' x xs where
  myAnd' acc [] = acc;
  myAnd' acc (y:ys) = myAnd' (acc && y) ys;

or' :: [Bool] -> Bool
or' (x:xs) = x || or' xs

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x == all' p xs

map' :: (a -> b) -> [a] -> [b]
map' p [] = []
map' p xs = [ p x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' p [] = []
map'' p (x:xs) = p x : map'' p xs


intersperse' :: a -> [a] -> [a]
intersperse' _ [x] = [x]
intersperse' n (x:xs) = x : n : intersperse' n xs

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' f (x:xs) = f x ++ (concatMap' f xs)

unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:xs) = x ++ "\n" ++ unlines' xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' p (x:xs) 
  | p x = let (t, f) = partition' p xs in (x:t, f)
  | otherwise = let (t, f) = partition' p xs in (t, x:f)

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):xs) = let (zs, as) = unzip' xs in (x:zs, y:as)  

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys) 
  | x < y     = x : y : ys 
  | otherwise = y : insert' x ys

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) = insert' x (sort' xs)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n xs@(head:tail) 
  | length xs < n  = xs
  | otherwise = head : take' (n-1) tail 

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs) 
  | p x       = x : takeWhile' p xs
  | otherwise = []

group' :: [a] -> [[a]]
group' [x] = [[x]]
group' (x:xs) 
  | 

main = print $ group' "Mississippi"
