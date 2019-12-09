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