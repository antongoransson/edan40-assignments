import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (return, fail)
import Parser
import qualified Dictionary
import Expr
import Statement

-- PARSER TESTS
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

-- EXPR TEST
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

p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11 :: Statement.T 
p1 = fromString "skip;"
p2 = fromString "read count;"
p3 = fromString "write count+1;"
p4 = fromString "count := 0;"
p5 = fromString "begin skip; end"
p6 = fromString "begin x:=0; x:=x+1; end"
p7 = fromString "if x then skip; else x:=0-x;"
p8 = fromString "while n do n:=n-1;"
s9 = "while n do begin fac:=fac*n; n:=n-1; end"
p9 = fromString s9
p10 = fromString  "begin read x ; x := x + 1 ; write x; end"
p11 = fromString  ("begin read n; fac:=1; " ++ s9 ++ " write fac; end")

tabSize = 2
indent = replicate tabSize ' '    

statementTest =  testGroup "statementTest"
    [ testCase "skip" $ Statement.toString p1 @?= "skip;\n"
    , testCase "read var" $ Statement.toString p2 @?= "read count;\n"
    , testCase "write var" $ Statement.toString p3 @?= "write count+1;\n"
    , testCase "assign var" $ Statement.toString p4 @?= "count:=0;\n"
    , testCase "begin" $ Statement.toString p5 @?= "begin\n" ++ indent ++ "skip;\nend\n"
    , testCase "begin with var" $ Statement.toString p6 @?= "begin\n" ++ indent ++ "x:=0;\n"++ indent ++ "x:=x+1;\nend\n"
    , testCase "if staetment" $ Statement.toString p7 @?= "if x then\n" ++ indent ++ "skip;\nelse\n" ++ indent ++ "x:=0-x;\n"
    
    ]     
allTests = testGroup "all tests"
    [ parserTests
    , exprTest
    , statementTest
    ]

main :: IO ()
main = defaultMain allTests