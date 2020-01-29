import Data.Set
import Control.Monad
-- Task1

move :: Int -> Int -> Maybe Int
move m p
    | abs(m + p) > 2    = Nothing
    | otherwise         = Just (m + p)

-- (Just 0) >>= move 1 >>= move (-2) >>= move 12

move_list :: [Int] -> Int -> Maybe Int
move_list xs pos = Prelude.foldr (\a b -> b >>= move a) (Just pos) xs

move_list' :: [Int] -> Int -> Maybe Int
move_list' xs pos = Prelude.foldl (\b a -> b >>= move a) (Just pos) xs


-- Task2

roll :: [(Integer, Integer)]
roll = do
    x <- [1..6]
    y <- [1..20]
    return (x,y)

roll' :: [(Integer, Integer)]
roll' = [1..20] >>= (\x -> [1..6] >>= (\y -> [(x,y)]))

roll'' = [(x,y) | x <- [1..20], y <- [1..6]]

-- Task 3

type KnightPos = (Int, Int)

moveKnight :: Int -> Int -> KnightPos -> [KnightPos]
moveKnight nc nr (c,r) = Prelude.filter (\x -> fst x `elem` [1..nc] &&  snd x `elem` [1..nr])
    [
        (c+2, r+1), (c+2, r-1),
        (c-2, r+1), (c-2, r-1),
        (c+1, r+2), (c+1, r-2),
        (c-1, r+2), (c-1, r-2)
    ]

moveKnight3 :: KnightPos -> Int -> Int -> [KnightPos]
moveKnight3 pos cn rn = return pos >>= moveKnight cn rn >>= moveKnight cn rn >>= moveKnight cn rn

moveKnightN :: Int -> KnightPos -> [KnightPos]
moveKnightN n pos = Prelude.foldl (\x _ -> clear (x >>= moveKnight 8 8)) (return pos) [1..n] where
                clear = toList . fromList


moveKnightN':: Int -> KnightPos -> [KnightPos]
moveKnightN' n pos = foldM (\b _ -> moveKnight 8 8 b) pos [1..n]


