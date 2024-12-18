module Main where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Array as A
import Data.Array (Array, (!))

testInput :: String
testInput = "2333133121414131402"

type Input = Array Int Block

newtype FId = FId Int deriving (Show)

newtype FSize = FSize Int deriving (Show)

data Block = File FId FSize | Free Int
    deriving (Show)

data PState = PFile | PFree

-- >>> parse testInput
-- array (0,18) [(0,File (FId 0) (FSize 2)),(1,Free 3),(2,File (FId 1) (FSize 3)),(3,Free 3),(4,File (FId 2) (FSize 1)),(5,Free 3),(6,File (FId 3) (FSize 3)),(7,Free 1),(8,File (FId 4) (FSize 2)),(9,Free 1),(10,File (FId 5) (FSize 4)),(11,Free 1),(12,File (FId 6) (FSize 4)),(13,Free 1),(14,File (FId 7) (FSize 3)),(15,Free 1),(16,File (FId 8) (FSize 4)),(17,Free 0),(18,File (FId 9) (FSize 2))]
parse :: String -> Input
parse s = parse' (FId 0) PFile s & arrayFromList
    where
        parse' _ _ "" = []
        parse' (FId n) PFile (c:rest) = File (FId n) (FSize $ read [c]):parse' (FId $ n+1) PFree rest
        parse' fid PFree (c:rest) = Free (read [c]):parse' fid PFile rest

arrayFromList :: [a] -> Array Int a
arrayFromList xs = A.listArray (0, length xs - 1) xs

expand :: Input -> Input
expand arr = expand' (A.elems arr) & arrayFromList
    where
        expand' ((File i (FSize n)):xs)
            | n >= 1 = File i (FSize 1):expand' (File i (FSize $ n-1):xs)
            | otherwise = expand' xs
        expand' ((Free n):xs)
            | n >= 1 = Free 1:expand' (Free (n-1):xs)
            | otherwise = expand' xs
        expand' [] = []

compact :: Input -> Input
compact arr = compact' arr start end
    where
        (start, end) = A.bounds arr
        compact' blocks lo hi
            | hi - lo < 1 = blocks
            | isFile blocks lo = compact' blocks (lo+1) hi
            | isFree blocks hi = compact' blocks lo (hi-1)
            | otherwise = compact' (swap blocks lo hi) lo hi
        isFile xs i = case xs ! i of
            (File _ _) -> True
            _ -> False
        isFree xs i = not $ isFile xs i
        swap xs i j = xs A.// [(i, xs ! j), (j, xs ! i)]

solve1 :: Input -> Int
solve1 input = expand input
    & compact
    & A.assocs
    & map (\(i, el) -> i * getVal el)
    & sum
    where
        getVal (File (FId x) (FSize 1)) = x
        getVal (Free 1) = 0
        getVal (File (FId _) (FSize _)) = error "inconsistent fsize > 1"
        getVal (Free _) = error "inconsistent free size > 1"

-- >>> showMatrix $ compact $ expand $ arrayFromList [(File (FId 0) (FSize 2)), (Free 2), (File (FId 1) (FSize 2))]
-- "0011.."
showMatrix :: Input -> String
showMatrix i = concatMap showCell (A.elems i)
    where
        showCell (Free _ ) = "."
        showCell (File (FId x) _) = show x

main :: IO ()
main = do
    let t = parse testInput
    putStrLn $ "test1: " ++ show (solve1 t)

    input <- readFile "inputs/day09" <&> parse
    putStrLn $ "solution1: " ++ show (solve1 input)
