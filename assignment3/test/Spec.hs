import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Prelude hiding (return, fail)
import Parser

letterTest = testGroup "parser test"
    [ testCase "abc" $ letter "abc" @?= Just('a',"bc")
    , testCase "digits " $ letter "123" @?= Nothing
    , testCase "empty " $ letter "" @?= Nothing
    ]

spacesTest = testGroup "spaces test"
    [ testCase "no spaces" $ spaces "abc" @?= Just("","abc")
    , testCase "space and tab" $ spaces "  \t abc" @?= Just("  \t ","abc")
    ]

charsTest = testGroup "chars test"
    [ testCase "2 chars" $ chars 2 "abc" @?= Just ("ab","c")
    , testCase "0 chars" $ chars 0 "ab" @?= Just ("","ab")
    , testCase "3 chars" $ chars 3 "ab" @?= Nothing
    ]

requireTest = testGroup "require test"
    [ testCase "require ok" $ require ":=" ":= 1" @?= Just (":=","1")
    -- , testCase "ete" null [1] @? "Non-empty list" {- Program error: expecting else near then -}
    ]

acceptTest = testCase "accept test" $ (accept "read" -# word) "read count" @?= Just ("count","")

parserTests = testGroup "all tests"
    [ letterTest
    , spacesTest
    , charsTest
    , requireTest
    , acceptTest
    ]

allTests = testGroup "all tests"
    [ parserTests
    ]

main :: IO ()
main = defaultMain allTests