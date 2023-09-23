data Tree a = Leaf | Node (Tree a) a (Tree a)

succOf :: Ord a => a -> Tree a -> Maybe a
succOf q Leaf = Nothing
succOf q (Node l x r)
  | q == x = Just x
  | q > x = succOf q r
  | q < x = case succOf q l of
    Just s -> Just s
    Nothing -> Just x
