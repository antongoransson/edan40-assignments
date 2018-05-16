module Main where

import Program
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

main :: IO ()
main = print (Program.exec p [3, 16])
