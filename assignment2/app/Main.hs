module Main where

    import StringAlignment
    
    main :: IO ()
    main = do
      let scoreMatch = 0
          scoreMismatch = -1
          scoreSpace = -1
          string1 = "writers"
          string2 = "vintner"
    
      outputOptAlignments string1 string2