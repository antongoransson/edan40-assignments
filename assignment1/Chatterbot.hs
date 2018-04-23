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
reflect = map refl
  where refl word | Just reflection <- lookup word reflections = reflection
                  | otherwise = word
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
prepare = words . map toLower .
          filter (not . flip elem ".,:;*!#%&|")--reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile  = map $ map2 (words. map toLower, map words)
--------------------------------------

plsd = ["you", "will", "never", "see", "your", "reflection", "in", "my", "eyes"]
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
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------
-- wildcard t s (char, string, string)
-- Replaces a wildcard in a list with the list given as the third argument
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
match w p s
  | head p == w      = orElse (singleWildcardMatch p s) (longerWildcardMatch p s)
  | head p == head s = match w (tail p) (tail s)
  | otherwise        = Nothing

-- The function singleWildcardMatch defines the case when the rest of the list matches with the rest of the pattern, i.e. the front wildcard removed
-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) $ match wc ps xs
-- The function longerWildcardMatch defines the case when rest of the list matches with the pattern with the wildcard retained at the front.
longerWildcardMatch (wc:ps) (x:xs) =  mmap (x:) $ match wc (wc:ps) xs

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

-- transformationApply wc f s p
--   | match wc (fst p) s == Nothing = Nothing
--   | otherwise =  Just $ substitute wc (snd p) $ maybe s id $ mmap f $ match wc (fst p) s
-- Applying a list of patterns until one succeeds
-- wc function patterns string
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (p:ps) s
  | transformationApply wc f s p == Nothing = transformationsApply wc f ps s--transformationApply wc f s p--val <- transformationApply wc f s p = val
  | otherwise                           =  transformationApply wc f s p
