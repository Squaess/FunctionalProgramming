data MTree a = Leaf a | Node a [MTree a]

instance (Show a) => Show (MTree a) where 
    show (Leaf a) = "<" ++ show a ++ ">"
    show (Node a xs) = show a ++ " [" ++ (foldl (\x y -> x ++ " " ++ (show y)) "" xs) ++ " ]"

instance Functor MTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node a xs) = Node (f a) (map (fmap f) xs)

instance Foldable MTree where
    foldr f z (Leaf x) = f x z
    foldr f z (Node a xs) = foldr (\x y -> foldr f y x) (f a (foldr (\x y -> foldr f y x) z t2)) t1 where
        split myList = splitAt (((length myList) + 1) `div` 2) myList
        (t1, t2) = split xs

count_n_l :: MTree a -> (Int, Int)
count_n_l (Leaf a) = (0, 1)
count_n_l (Node a xs) = foldr (\(x1, x2) (y1, y2) -> (x1+y1, x2+y2)) (1,0) $ map count_n_l xs

contains :: Eq a => a -> MTree a -> Bool
contains a (Leaf x) = a == x
contains y (Node x xs) = y == x || foldr (||) False [contains y j |j <- xs]

height :: MTree a -> Int
height (Leaf _) = 0
height (Node _ xs) = 1 + foldr (\x y -> max (height x) y) 0 xs