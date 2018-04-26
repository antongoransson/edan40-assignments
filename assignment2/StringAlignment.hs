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

similarityScore :: String -> String -> Int
similarityScore [] _ = scoreSpace
similarityScore _ [] = scoreSpace
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score (x, y),
                              similarityScore (x:xs) ys + score ('-', y),
                              similarityScore xs (y:ys) + score (x, '-')]

--Appends(:) h1 to the first lits and h2 to the seconds list, for all pairs in a list.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

--maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f xs = a f xs []
  where a f (x:xs) maxs | null xs = maxs
                        | null maxs || f x > (f . head) maxs = a f xs [x]
                        | f x == (f . head) maxs = a f xs (x:maxs)
                        | otherwise = a f xs maxs


optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = maximaBy stringScore $ a xs ys
  where
    a [] [] = [("","")]
    a [] ys = attachHeads '-' (head ys) [("","")]
    a xs [] = attachHeads (head xs) '-' [("","")]
    a (x:xs) (y:ys) = concat [
      attachHeads x y $ a xs ys,
      attachHeads x '-' $ a xs (y:ys),
      attachHeads '-' y $ a (x:xs) ys
      ]


-- sim((x:xs),(y:ys)) = max {sim(xs,ys) + score(x,y),
--                           sim(xs,(y:ys)) + score(x,'-'),
--                           sim((x:xs),ys) + score('-',y)}
score :: (Char, Char) -> Int
score (x, '-')  = scoreSpace
score ('-', y) = scoreSpace
score (x, y)
  | x == y    = scoreMatch
  | otherwise = scoreMismatch



string1 = "writers"
string2 = "vintner"
