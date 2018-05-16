module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
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

statement = comment ! skip ! assignment ! begin ! ifElse ! while ! read' ! write

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require "skip;" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter (parse #- spaces) #- require "end;" >-> Begin

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (a, b) = While a b

read' = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

ifElse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((a, b), c) = If a b c

comment = accept "--" -# nextLine -# require "\n" >-> Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if Expr.value cond dict > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment v e : stmts) dict input = exec stmts (Dictionary.insert(v, Expr.value e dict) dict) input    
exec (While cond wStmts : stmts) dict input = 
    if Expr.value cond dict > 0 
    then exec (wStmts: While cond wStmts : stmts) dict input
    else exec stmts dict input
exec (Begin bStmts : stmts) dict input = exec (bStmts ++ stmts) dict input
exec (Read s : stmts) dict (i:input) = exec stmts (Dictionary.insert(s, i) dict) input 
exec (Write e: stmts) dict input =  Expr.value e dict : exec stmts dict input 
exec (Skip : stmts) dict input = exec stmts dict input
exec (Comment s : stmts) dict input = exec stmts dict input
instance Parse Statement where
  parse = statement
  toString = error "Statement.toString not implemented"

