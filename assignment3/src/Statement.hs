module Statement(T, parse, toString, fromString, exec) where
import Prelude
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Begin [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Skip |
    Comment String
    deriving Show

statement = commentStatement ! skipStatement ! assignmentStatement ! beginStatement ! ifStatement ! whileStatement ! readStatement ! writeStatement

assignmentStatement = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skipStatement = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

beginStatement = accept "begin" -# iter (parse #- spaces) #- require "end" >-> Begin

whileStatement = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (a, b) = While a b

readStatement = accept "read" -# word #- require ";" >-> Read

writeStatement = accept "write" -# Expr.parse #- require ";" >-> Write

ifStatement = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((a, b), c) = If a b c

commentStatement = accept "--" -# nextLine #- require "\n" >-> Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input
    | Expr.value cond dict > 0 = exec (thenStmts: stmts) dict input
    | otherwise                = exec (elseStmts: stmts) dict input
exec (While cond wStmts : stmts) dict input
    | Expr.value cond dict > 0 = exec (wStmts: While cond wStmts : stmts) dict input
    | otherwise                = exec stmts dict input
exec (Assignment v e : stmts) dict input = exec stmts (Dictionary.insert(v, Expr.value e dict) dict) input
exec (Begin bStmts : stmts) dict input = exec (bStmts ++ stmts) dict input
exec (Read s : stmts) dict (i:input) = exec stmts (Dictionary.insert(s, i) dict) input
exec (Write e : stmts) dict input =  Expr.value e dict : exec stmts dict input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Comment s : stmts) dict input = exec stmts dict input

indentSize = 2
indents n = replicate (n * indentSize ) ' '

shw ::Int -> Statement -> String
shw n (If cond thenStmts elseStmts) = indents n ++ "if " ++ Expr.toString cond ++    " then\n" ++ shw (n + 1) thenStmts ++ indents n ++ "else\n" ++ shw (n + 1) elseStmts
shw n (Assignment v e) = indents n ++ v ++ ":=" ++ Expr.toString e ++ ";\n"
shw n (While cond wStmts) = indents n ++ "while " ++ Expr.toString cond ++ " do\n" ++ shw (n + 1) wStmts
shw n (Begin bStmts) = indents n ++ "begin\n" ++ concatMap (shw (n + 1)) bStmts ++ indents n ++ "end\n"
shw n (Read s) = indents n ++ "read " ++ s ++ ";\n"
shw n (Write e) = indents n ++ "write " ++ Expr.toString e ++ ";\n"
shw n Skip = indents n ++ "skip" ++ ";\n"
shw n (Comment s) = "-- " ++ s ++ "\n"

instance Parse Statement where
    parse = statement
    toString = shw 0
