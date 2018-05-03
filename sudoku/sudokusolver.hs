module SodukuSolver where
import qualified Data.HashMap.Strict as M
import qualified Data.Set as Set

cross:: String -> String -> [String]
cross xs ys = [x : [y] | x <- xs, y <- ys]

digits = "123456789"

rows = "ABCDEFGHI"

cols = digits
squares = cross rows cols

unitlist:: [[String]]
unitlist = [cross rows [c] | c <- cols] ++
           [cross [r] cols | r <- rows] ++
           [cross rs cs | rs <- ["ABC","DEF","GHI"], cs <- ["123","456","789"]]


units = M.fromList [(s, [u | u <- unitlist, s `elem` u]) | s <- squares]
peers = M.fromList [(s, Set.delete s (Set.fromList (concat(units M.! s)))) | s <- squares]

grid = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"

values = M.fromList[(s, digits)  | s <- squares]
--Convert grid to a dict of possible values, {square: digits}, or return False if a contradiction is detected.
-- parseGrid =
--     for s,d in grid_values(grid).items():
--         if d in digits and not assign(values, s, d):
--     return values
gridValues grid = M.fromList $ zip squares [[c] | c <- grid , c `elem` digits || c `elem` "0."]
--["A2", "B2", "D2", "E2", "F2", "G2", "H2", "I2", "C1", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "A1", "A3", "B1", "B3"]
-- assign grid square digit =
-- eliminate values square digit
--   | digit `notElem` ( values M.! square) = values
--   | null( values M.! square) = M.fromList []
--   | length (values M.! square) == 1 = M.fromList []
--   | otherwise= M.fromList []
