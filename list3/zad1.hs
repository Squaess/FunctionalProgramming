data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

create_leaf :: a -> Tree a
create_leaf x = Leaf x

create_node :: a -> a -> a -> Tree a
create_node left root right = Node (Leaf left) root (Leaf right)

concat_trees :: Tree a -> a -> Tree a -> Tree a
concat_trees left root right = Node left root right

b_conn = Node

instance Foldable Tree where
    foldr f z (Leaf x) = f x z
    foldr f z (Node left root right) = foldr f (f root (foldr f z right)) left

count_n_l :: Tree a -> (Int, Int)
count_n_l (Leaf a) = (0, 1)
count_n_l (Node l x r) = (lN + 1 + rN, lL+rL) where
    (lN, lL) = count_n_l l
    (rN, rL) = count_n_l r

instance Functor Tree where
  fmap f (Leaf x)       = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

contains :: Eq a => a -> Tree a -> Bool
contains x (Leaf a) = x == a
contains a (Node l x r) = a == x || contains a l || contains a r

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l _ r) = max (height l) (height r) + 1