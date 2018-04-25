module StringAlignment where

type AlignmentType = (String,String)
--Observe that every alignment can start in exactly one of three ways:
--1 Two non-space characters
--2 Non-space above, space below
--3 Space above, non-space below
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments _ _ _ _ _= [("ABC ","ACBC ")]

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore (s1:ss1) (s2:ss2) = score (s1, s2) + similarityScore ss1 ss2

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

--maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ _= []



optAlignments :: String -> String -> [AlignmentType]
optAlignments _ _ = [("","")]

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
