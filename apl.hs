import Data.Array
import Data.Function
import Control.Monad
import Data.List
import Data.Maybe
import Control.Arrow
import Data.List.Split
import Data.Functor
import System.Random

-- Helper functions.
(/.) :: (Integral a, RealFrac b) => a -> a -> b
(/.) = (/) `on` fromIntegral

tr :: (Eq a, Enum a) => [a] -> [b] -> b -> [a] -> [b]
tr from to def = map (\c -> fromMaybe def (lookup c (zip from to)))

-- Dyalog APL Competition: 2013, Phase 1.
d2013p1 :: (Num a, Enum a) => a -> [a] -- ⍳+⍳-≢
d2013p1 n = [2 * n + 1 | n <- [0 .. n]]

d2013p2 :: (Ord a, Num a) => [a] -> Double -- 100×+.≤÷≢⍤⊢
d2013p2 n = 100 * length (filter (> 65) n) /. length n

d2013p3 :: String -> Int -- ≢' '∘≠⊆,
d2013p3 = length . words

d2013p4 :: String -> Bool -- {((¯1∊+\)⍱0≠+/)1 ¯1 0['()'⍳⍵]}
d2013p4 s = all (>= 0) p && (last p == 0)
    where p = scanl1 (+) (tr "()" [1, -1] 0 s)

d2013p5 :: Integral a => a -> [[a]] -- ∘.=⍨⍳
d2013p5 n = [[fromIntegral (fromEnum (i == j)) | j <- [1 .. n]] | i <- [1 .. n]]

d2013p6 :: Integral a => [a] -> a -- ⌈/-⌊/
d2013p6 = uncurry (-) . (maximum &&& minimum)

d2013p7 :: RealFrac a => [a] -> [a] -- /⍨∘(≠∘⌊⍨)⍨
d2013p7 = filter (not . isInt)
    where isInt x = x == fromIntegral (round x)

d2013p8 :: Integral a => a -> [[a]] -- ∘.×⍨⍳
d2013p8 n = [[i * j | j <- [1 .. n]] | i <- [1 .. n]]

d2013p9 :: Fractional a => [a] -> Int -> [a] -- ⊣÷⍨⊢+⌿⍨⊣⌊≢⍤,
d2013p9 xs n = chunksOf n xs <&> average
    where
        average :: Fractional a => [a] -> a
        average xs = sum xs / fromIntegral (length xs)

d2013p10 :: (Fractional f, Eq f) => [[f]] -> [f] -> [f] -- ⌹
d2013p10 a b = resubstitute $ triangular (zipWith (++) a (map (: []) b))
    where resubstitute = reverse . resubstitute' . reverse . map reverse

triangular :: (Fractional f, Eq f) => [[f]] -> [[f]]
triangular [] = []
triangular m = row : triangular rows'
    where
    (row : rows) = rotatePivot m
    rows' = map f rows
    f bs | head bs == 0 = drop 1 bs
         | otherwise = drop 1 $ zipWith (-) (map (* c) bs) row
         where c = head row / head bs
    rotatePivot (row:rows) | head row /= 0 = row:rows | otherwise = rotatePivot (rows ++ [row])

resubstitute' :: Fractional f => [[f]] -> [f]
resubstitute' [] = []
resubstitute' (row:rows) = x : resubstitute' rows'
    where
    x = head row / last row
    rows' = map substituteUnknown rows
    substituteUnknown (a1 : (a2 : as')) = (a1 - x * a2) : as'

-- Dyalog APL Competition: 2014, Phase 1.
d2014p1 :: (Eq a, Num a) => [a] -> Bool -- +.×⍨⍤⊣=×⍨⍤⊢
d2014p1 [a, b, c] = a^2 + b^2 == c^2

d2014p2 :: String -> String -- {1↓∊{' ',⍵/⍨1@1(≢⍵)~'AEIOU'∊⍨1⎕C⍵}¨⍵⊆⍨' '≠⍵}
d2014p2 = unwords . map f . words
    where f [x] = [x]
          f (x:xs) = x : filter (`notElem` "aeiou") (init xs) ++ [last xs]

d2014p3 :: Integral a => a -> [a] -- {⍵≤1:⍵ ⋄ +/∇¨⍵-⍳2}¨⍳
d2014p3 n = take (fromIntegral n) $ 0 : 1 : zipWith (+) fibs (tail fibs)
    where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

d2014p4 :: String -> String -- ' '∘(=∘⊃↓⊢⊢⍤/⍨2∨/0,⍨≠)
d2014p4 = unwords . words

d2014p5 :: String -> Bool -- ≡∘⌽⍨∩⍥⎕C∘⎕A
d2014p5 = (==) <*> reverse . filter (`elem` ['a'..'z'])

d2014p6 :: [Int] -> [Int] -- {,∘≢⌸,+/¨⍳⍵}
d2014p6 x = map length (group (sort (map sum (helper x))))
    where
        helper [] = [[]]
        helper (x:xs) = [y:ys | y <- [1..x], ys <- helper xs]

d2014p7 :: Integral a => a -> a -> a -- ⊣÷∨
d2014p7 = liftM2 (.) div gcd

d2014p8 :: Floating a => [a] -> [a] -> a -- {0.5*⍨+/2*⍨⍺-⍵}
d2014p8 = ((sqrt . sum . map (** 2)) .) . zipWith (-)

d2014p9 :: Floating a => a -> a -> a
d2014p9 vel traj = vel^2 * sin (2 * traj) / 9.81

d2014p10 :: (Floating a, Ord a) => [a] -> a -- 100×⌈⌿(¯2-⌿⍵)÷¯1↓⍵
d2014p10 vec = 100 * maximum (zipWith (/) (chunksOf 2 vec <&> (\[a, b] -> b - a)) (init vec))

