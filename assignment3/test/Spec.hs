import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (return, fail)
import Parser
import qualified Dictionary
import Expr
import Statement
import Program

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

-- STATEMENTTESTS
s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11 :: Statement.T 
s1 = fromString "skip;"
s2 = fromString "read count;"
s3 = fromString "write count+1;"
s4 = fromString "count := 0;"
s5 = fromString "begin skip; end"
s6 = fromString "begin x:=0; x:=x+1; end"
s7 = fromString "if x then skip; else x:=0-x;"
s8 = fromString "while n do n:=n-1;"
ss9 = "while n do begin fac:=fac*n; n:=n-1; end"
s9 = fromString ss9
s10 = fromString  "begin read x ; x := x + 1 ; write x; end"
s11 = fromString  ("begin read n; fac:=1; " ++ ss9 ++ " write fac; end")

tabSize = 2
indent = replicate tabSize ' '    
statementTest =  testGroup "statementTest"
    [ testCase "skip" $ Statement.toString s1 @?= "skip;\n"
    , testCase "read var" $ Statement.toString s2 @?= "read count;\n"
    , testCase "write var" $ Statement.toString s3 @?= "write count+1;\n"
    , testCase "assign var" $ Statement.toString s4 @?= "count:=0;\n"
    , testCase "begin" $ Statement.toString s5 @?= "begin\n" ++ indent ++ "skip;\nend\n"
    , testCase "begin with var" $ Statement.toString s6 @?= "begin\n" ++ indent ++ "x:=0;\n"++ indent ++ "x:=x+1;\nend\n"
    , testCase "if statement" $ Statement.toString s7 @?= "if x then\n" ++ indent ++ "skip;\nelse\n" ++ indent ++ "x:=0-x;\n"
    , testCase "while begin statement" $ Statement.toString s9 @?= "while n do\n" ++ indent ++ "begin\n" ++ indent ++ indent ++ "fac:=fac*n;\n" ++ indent ++ indent ++ "n:=n-1;\n" ++ indent ++ "end\n"
    , testCase "begin assignments" $ Statement.toString s10 @?= "begin\n" ++ indent ++ "read x;\n" ++ indent ++ "x:=x+1;\n" ++ indent ++ "write x;\nend\n"
    , testCase "begin assignments" $ Statement.toString s11 @?= "begin\n" ++ indent ++ "read n;\n" ++ indent ++ "fac:=1;\n" ++ indent ++ "while n do\n" ++ indent ++ indent ++ "begin\n" ++ indent ++ indent ++ indent ++  "fac:=fac*n;\n" ++ indent ++ indent ++ indent ++ "n:=n-1;\n" ++ indent ++ indent ++ "end\n"  ++ indent ++ "write fac;\nend\n"
    ]    

-- PROGRAM TESTS
p, p1 :: Program.T
p = fromString  "\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end"

p1 = fromString  "\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;"

p4 = fromString  "\
\read a;\
\read b;\
\-- a comment\n\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\                    
\    a := a-1;\
\  end\
\write a;"

programTest =  testGroup "programtest"
    [ testCase "program p [3,16]" $ Program.exec p [3,16] @?= [3, 6, 9, 12, 15]
    , testCase "program p4 [4, 4]" $  Program.exec p4 [4,4] @?= [64, 16, 27, 8, 8, 4, 1, 2, 0]
    , testCase "program p2 [1024, 2]" $ Program.exec p1 [1024, 2] @?= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 10000000000]
    ]

allTests = testGroup "all tests"
    [ parserTests
    , exprTest
    , statementTest
    , programTest
    ]

main :: IO ()
main = defaultMain allTests