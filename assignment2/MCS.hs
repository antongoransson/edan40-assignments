module MaximumLO where

mcsLength1 :: Eq a => [a] -> [a] -> Int
mcsLength1 _ [] = 0
mcsLength1 [] _ = 0
mcsLength1 (x:xs) (y:ys)
  | x == y    = 1 + mcsLength1 xs ys
  | otherwise = max (mcsLength1 xs (y:ys)) (mcsLength1 (x:xs) ys)

-- mcsLength1 "abcdefghijk" "bcdefghijkl"
-- 11 characters
-- a second

-- mcsLength1 "abcdefghijklmn" "bcdefghijklmno"
-- 14 characters
-- ten seconds

-- mcsLength1 "abcdefghijklmnop" "bcdefghijklmnopq"
-- 16 characters
-- eternity

mcsLength :: Eq a => [a] -> [a] -> Int

mcsLength xs ys = mcsLen (length xs) (length ys)
  where
     mcsLen i j = mcsTable!!i!!j
     mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

     mcsEntry :: Int -> Int -> Int
     mcsEntry _ 0 = 0
     mcsEntry 0 _ = 0
     mcsEntry i j
       | x == y    = 1 + mcsLen (i-1) (j-1)
       | otherwise = max (mcsLen i (j-1)) (mcsLen (i-1) j)
       where
          x = xs!!(i-1)
          y = ys!!(j-1)
