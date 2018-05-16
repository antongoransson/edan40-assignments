import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (return, fail)
import Parser
import qualified Dictionary
import Expr

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
    ]

acceptTest = testCase "accept test" $ (accept "read" -# word) "read count" @?= Just ("count","")
nextLineTest = testGroup "comment test"
    [ testCase "ignore comment ok" $ (spaces #-nextLine -# require "\n")  "ADsdsadsadasdasdas\n" @?= Just ("\n","")
    ]
parserTests = testGroup "all tests"
    [ letterTest
    , spacesTest
    , charsTest
    , requireTest
    , acceptTest
    , nextLineTest
    ]

-- n21 = testValue "1/(2-y)" {-  Expr.value: division by 0 -}
-- n31 = testValue "2+z"     {-  Expr.value: undefined variable z -}
x = 1
y = 2
dict = Dictionary.insert ("x", 1) $ 
       Dictionary.insert ("y", 2) $
       Dictionary.empty 

testValue string = value (fromString string) dict    
exprTest = testGroup "exprTest" 
    [ testCase "1 integer" $ testValue "1"  @?= 1
    , testCase "x" $ testValue "x"  @?= x
    , testCase "x+y" $ testValue "x+y"  @?= x + y
    , testCase "x-y-y" $ testValue "x-y-y"  @?= x - y -y
    ]
allTests = testGroup "all tests"
    [ parserTests
    , exprTest
    ]

main :: IO ()
main = defaultMain allTests