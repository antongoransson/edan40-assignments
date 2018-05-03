module StringAlignment where

type AlignmentType = (String,String)
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1


stringScore :: (String, String) -> Int
stringScore ([], []) = 0
stringScore (x:xs, y:ys) = score (x, y) + stringScore (xs, ys)

outputOptAlignments :: String -> String -> IO()
outputOptAlignments xs ys = do
  let x = optAlignments xs ys
  putStrLn ""
  (putStr.unlines) $ map unpackPairs x
  putStrLn $ "Number of optimal alignments: "  ++  (show . length) x

unpackPairs :: (String, String) -> String
unpackPairs (a, b) = addSpace a ++ "\n" ++ addSpace b ++ "\n"

addSpace :: String -> String
addSpace = concatMap (:[' '])

similarityScoreSlow :: String -> String -> Int
similarityScoreSlow [] _ = scoreSpace
similarityScoreSlow _ [] = scoreSpace
similarityScoreSlow (x:xs) (y:ys) = maximum [
                              similarityScoreSlow xs ys + score (x, y),
                              similarityScoreSlow (x:xs) ys + score ('-', y),
                              similarityScoreSlow xs (y:ys) + score (x, '-')
                              ]

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
          x = xs !! (i - 1)
          y = ys !! (j - 1)

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs, ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f xs = findMaxs f xs []
  where
    findMaxs _ [] maxs = maxs
    findMaxs f (x:xs) maxs   | null maxs || f x > (f . head) maxs = findMaxs f xs [x]
                             | f x == (f . head) maxs             = findMaxs f xs (x:maxs)
                             | otherwise                          = findMaxs f xs maxs

optAlignmentsSlow :: String -> String -> [AlignmentType]
optAlignmentsSlow xs ys = maximaBy stringScore $ findAlignments xs ys
  where
    findAlignments [] [] = [("","")]
    findAlignments [] ys = attachHeads '-' (head ys) [("","")]
    findAlignments xs [] = attachHeads (head xs) '-' [("","")]
    findAlignments (x:xs) (y:ys) = concat [
      attachHeads x y $ findAlignments xs ys,
      attachHeads x '-' $ findAlignments xs (y:ys),
      attachHeads '-' y $ findAlignments (x:xs) ys
      ]

optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = snd $ optAlign (length xs) (length ys)
  where
     optAlign i j = optTable !! i !! j
     optTable = [[ optEntry i j | j <- [0..]] | i <- [0..] ]

     optEntry :: Int -> Int -> (Int, [AlignmentType])
     optEntry 0 0 = (0, [("", "")])
     optEntry i 0 = (i * scoreSpace, [(take i xs, replicate i '-')])
     optEntry 0 j = (j * scoreSpace, [(replicate j '-', take j ys)])
     optEntry i j = ((fst . head) findpath, concatMap snd findpath )
      where
          findpath  = maximaBy fst [
             calcScore x y   (optAlign (i - 1) (j - 1)),
             calcScore '-' y (optAlign i (j - 1)),
             calcScore x '-' (optAlign (i - 1) j)
             ]
          calcScore x y alignments = (fst alignments + score (x,y), attachTails x y (snd alignments))
          x = xs !! (i - 1)
          y = ys !! (j - 1)


score :: (Char, Char) -> Int
score (x, '-') = scoreSpace
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
