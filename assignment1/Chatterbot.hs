module Chatterbot where
import Utilities
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

-- ["Do you really think I don't * ?", "Perhaps eventually I will * .", "Do you really want me to * ?"]
--------------------------------------------------------
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
                 r <- randomIO :: IO Float
                 return $ rulesApply (map (map2 (id, pick r)) brain)

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply transformations phrase =  maybe [] id $ transformationsApply "*" reflect transformations phrase
reflect :: Phrase -> Phrase
reflect = map $ try (flip lookup reflections)

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce.words . map toLower .
          filter (not . flip elem ".,:;*!#%&|")--reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile  = map $ map2 (words. map toLower, map words)
--------------------------------------

reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply reductions = fix $ try $ transformationsApply "*" id reductions

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute w t s =  concatMap replace  t
  where replace x | x == w = s
                  | otherwise = [x]

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] s = Nothing
match _ p [] = Nothing
match w (p:ps) (s:ss)
  | p == w    = orElse (singleWildcardMatch (p:ps) (s:ss)) (longerWildcardMatch (p:ps) (s:ss))
  | p == s    = match w ps ss
  | otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) $ match wc ps xs
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) $ match wc (wc:ps) xs

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------
-- Applying a single pattern wc function string pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) ->  Maybe [a]
transformationApply wc f s p
  | Just m <- match wc (fst p) s = Just $ substitute wc (snd p) $ f m
  | otherwise                    = Nothing

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (p:ps) s = orElse (transformationApply wc f s p) (transformationsApply wc f ps s)
