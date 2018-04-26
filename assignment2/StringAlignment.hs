module StringAlignment where

type AlignmentType = (String,String)
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1


stringScore :: (String, String) -> Int
stringScore ([], []) = 0
stringScore ((s1:xs1), (s2:xs2)) = score (s1, s2) + stringScore (xs1, xs2)


optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments _ _ _ _ _= [("ABC ","ACBC ")]

-- similarityScore :: String -> String -> Int
-- similarityScore [] _ = scoreSpace
-- similarityScore _ [] = scoreSpace
-- similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score (x, y),
--                               similarityScore (x:xs) ys + score ('-', y),
--                               similarityScore xs (y:ys) + score (x, '-')]


similarityScore :: String -> String -> Int
similarityScore xs ys = simScore (length xs) (length ys)
  where
     simScore i j = simscoreTable !! i !! j
     simscoreTable = [[ simEntry i j | j <- [0..]] | i <- [0..] ]

     simEntry :: Int -> Int -> Int
     simEntry 0 0 = 0
     simEntry _ 0 = scoreSpace
     simEntry 0 _ = scoreSpace
     simEntry i j
       | x == y    = scoreMatch + simScore (i - 1) (j - 1)
       | otherwise = max (scoreMismatch + simScore i (j - 1)) (scoreMismatch + simScore (i - 1) j)
       where
          x = xs!!(i - 1)
          y = ys!!(j - 1)

--Appends(:) h1 to the first lits and h2 to the seconds list, for all pairs in a list.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1], ys++[h2]) | (xs, ys) <- aList]

--maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f xs = a f xs []
  where
    a _ [] maxs = maxs
    a f (x:xs) maxs   | null maxs || f x > (f . head) maxs = a f xs [x]
                      | f x == (f . head) maxs = a f xs (x:maxs)
                      | otherwise = a f xs maxs

optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = optAlign xs ys (length xs) (length ys)
  where
     optAlign xs ys i j = optTable !! i !! j
     optTable = [[ optEntry xs ys i j | j <- [0..]] | i <- [0..] ]

     optEntry :: String-> String -> Int -> Int -> [AlignmentType]
     optEntry xs ys 0 0 = [("", "")]
     optEntry xs ys i 0 = attachTails (head xs) '-' $ optAlign xs ys (i - 1) 0
     optEntry xs ys 0 j = attachTails  '-' (head ys) $ optAlign xs ys 0 (j - 1)
     optEntry xs ys i j =  maximaBy stringScore $ concat [
       attachTails x y $ optAlign xs ys (i - 1) (j - 1),
       attachTails '-'y $ optAlign xs ys i (j - 1),
       attachTails x '-'  $ optAlign xs ys (i - 1) j
      ]
      where
        x = xs!!(i - 1)
        y = ys!!(j - 1)

score :: (Char, Char) -> Int
score (x, '-')  = scoreSpace
score ('-', y) = scoreSpace
score (x, y)
  | x == y    = scoreMatch
  | otherwise = scoreMismatch



string1 = "writers"
string2 = "vintner"
string3 = "bananrepubliksinvasionsarmestabsadjutant"
string4 = "kontrabasfiolfodralmakarmästarlärling"
string5 = "hejhejhejhej"
string6 = "hejhejhejhej"
