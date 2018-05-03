module TestPJ where

allTests :: [(String, [String])]
allTests
  =  [	  ("tst1", tst1)
	, ("tst2", tst2) 
	, ("tst3", tst3) 
	, ("tst4", tst4) 
	, ("tst5", tst5) 
	, ("tst8", tst8) 
	, ("tst9", tst9) 
	, ("tst10", tst10) 
	, ("tst11", tst11) 
	, ("tst12", tst12) 
	, ("tst13", tst13) 
	, ("tst14", tst14) 
	, ("xwing1", xwing1) 
	, ("xwing2", xwing2) 
	, ("xwing3", xwing3) 
	, ("xwing4", xwing4) 
	, ("swordfish1", swordfish1) 
	, ("swordfish2", swordfish2) 
	, ("swordfish3", swordfish3) 
	, ("swordfish4", swordfish4) 
	, ("swordfish5", swordfish5) 
	, ("swordfish6", swordfish6) 
	, ("swordfish7", swordfish7) 
	, ("rubylips1", rubylips1) 
	, ("rubylips2", rubylips2) 
	, ("rubylips3", rubylips3) 
	, ("rubylips4", rubylips4) 
	, ("rubylips5", rubylips5) 
	, ("rubylips6", rubylips6) 
	, ("rubylips7", rubylips7) 
	, ("rubylips8", rubylips8) 
	, ("rubylips9", rubylips9) 
	, ("rubylips10", rubylips10) 
	, ("solverVH1", solverVH1) 
	, ("indp1", indp1) 
	, ("gordon1", gordon1)
	]

hardTests
  =  [	  ("rubylips1", rubylips1) 
	, ("rubylips2", rubylips2) 
	, ("rubylips4", rubylips4) 
	, ("rubylips5", rubylips5) 
	, ("rubylips7", rubylips7) 
	, ("rubylips9", rubylips9) 
	]




-----------------------
-- One from http://www.csse.uwa.edu.au/~gordon/sudokumin.php 
-- Only 17 clues
-- Gets stuck after 12 moves
gordon1 = ["......1.",
	   "4",
	   ".2",
	   "....5.6.4",
	   "..8...3..",
	   "..1.9....",
	   "3..4..2..",
	   ".5.1",
	   "...8.7..."]

-----------------------
-- Independent
indp1 = ["6257.3...",
	"873....4.",
	"........3",
	"3.8...2..",
	".9...5...",
	".4....8..",
	"..6......",
	"431..796.",
	".5..21.3."]

-----------------------
-- Times magazine
-- Stuck after 54 moves
tst1 = [".5.1.....", 
	"4..6.2...", 
	".6...817.",
	"74.8.....",
	".........",
	".....3.59",
	".137...2.",
	"...4.6..8",
	".....5.9."]
	
-- Times; success after 103 moves
tst2 = ["3....9.5.",
	".67.1....",
	"2.....7..",
	".7.9.....",
	".9.4.1.3.",
	".....6.8.",
	"..4.....9",
	"....6.82.",
	".5.3....1"]

-- www.saidwhat.co.uk; difficult
-- Easy actually; solved in 89 moves
tst3 = ["....8.132",
	".........",
	"6...45.79",
	"..28.76..",
	"..1...5..",
	"..45.19..",
	"23.61...8",
	".........",
	"796.3...."]

-- www.saidwhat.co.uk; very hard
-- Easy actually; solved in 91 moves
tst4 = ["..9..3..2",
	"...4.1..8",
	"..5.....4",
	".3..4..7.",
	".8..9..2.",
	".1..6..5.",
	"7.....6..",
	"4..8.7...",
	"3..2..1.."]
	
-- www.saidwhat.co.uk; very hard
-- Solved in 112 moves
tst5 = ["..2974...",
	".......57",
	".........",
	"..4.5...2",
	"..9.1.6..",
	"8...3.4..",
	".........",
	"13.......",
	"...6829.."]

-- websudoku.com (evil)
-- Solved in 98 moves
tst8 = ["5..6.2...",
	".......6.",
	".62..3.54",
	".1...8...",
	".48...17.",
	"...2...3.",
	"49.8..71.",
	".5.......",
	"...1.4..6"]
	
-- websudoku.com (evil)
-- Solved in 99 moves
tst9 = [".91.2....",
	"..6.1..3.",
	"2....5...",
	".....29.4",
	"...176...",
	"6.34.....",
	"...9....7",
	".3..5.8..",
	"....8.25."]
	

-- easy
tst10 = ["85......2",
	".....164.",
	"2..47...8",
	"3.86..7..",
	".6.......",
	".79.8..5.",
	".....2..4",
	"...5.936.",
	"9........"]

-- Cambridge evening news (easy)
tst11 = ["...4.2...",
	"784.1....",
	"5....6..3",
	"2..83...1",
	".....4.6.",
	".5.....9.",
	".....1...",
	".9.6.....",
	"1657..94."]

-- Sudoko book (easy)
tst12 = ["...2.7...",	"...189...", ".9.....3.",
	"..6...1..", "5.......3", "413...589",
	".4.8.3.6.", ".87...39.", "..54.67.."]

-- Sudoko book (ultimate)
tst13 = ["2", "9.325..4.", "5....96.1",
	"...462..3", "", "...513..9",
	"8....13.7", "3.497..8", "1"]

tst14 = ["", "......147", "...73.9.6",
	"..9..6.1.", "..8.736..", "...92.5..",
	".37.58.9.", ".2.1..3..", ".14"]

-- Cambridge Evening news
tst15 = ["...7...84", "3.85.61", "4...9",
	"5......9", ".....7", "8.3..254",
	"...83", "..2..9..3", "..5.6.4"]


-- sadman sudoku xwing2
-- http://www.simes.clara.co.uk/programs/sudokutechnique6.htm
-- Stuck after 20 moves
-- Needs X-wing
xwing1 = ["...13...5",
	".4....2..",
	"8..9.....",
	"....5.9..",
	"..2...4..",
	"..3.6....",
	".....3..6",
	"..5....1.",
	"7...28..."]

xwing2 = ["..18..6..", "5........", "...79....",
	".73......", ".8.9.4.1.", "......29.",
	"....15...", "........3", "..6..24.."]

xwing3 = [".8...5..3", "7..2.....", "......6..",
	"....4.1.8", "..6...9..", "2.3.7....", 
	"..9......", ".....1..4", "5..8...2."]

xwing4 = [
    "..6......",
    "1.......3",
    ".4..7..2.",
    ".2.3....9",
    "...145...",
    "4....8.6.",
    ".7..9..8.",
    "3.......1",
    "......4.."]

swordfish1 = [
    "...47.6..",
    "..4...3.5",
    "92.......",
    ".31......",
    "...936...",
    "......28.",
    ".......16",
    "4.8...9..",
    "..7.52..."]

swordfish2 = [
    ".1....8.3",
    "5...96...",
    "..4....6.",
    "9..4.3...",
    ".2.....1.",
    "...8.5..7",
    ".6....4..",
    "...17...5",
    "1.3....2."]

swordfish3 = [
    "..8.9.1.5",
    ".316.....",
    "4........",
    ".....5...",
    "..3.1.6..",
    "...4.....",
    "........7",
    ".....742.",
    "8.9.6.3.."]

swordfish4 = [
    ".8....61.",
    "1...9.2..",
    "2....5..4",
    "9......7.",
    "...326...",
    ".4......1",
    "6..8....3",
    "..3.1...5",
    ".17....9."]

swordfish5 = [
    "..2..537.",
    "7..9....8",
    "61.......",
    "..8.1....",
    ".7..3..5.",
    "....2.4..",
    ".......32",
    "5....6..7",
    ".417..9.."]

swordfish6 = [
    "8......59",
    ".6..2...4",
    "5....17..",
    ".5....3..",
    "...497...",
    "..2....6.",
    "..85....3",
    "7...6..8.",
    "43......1"]

swordfish7 = [
    "5.......4",
    ".89..3.7.",
    ".672.....",
    "..3.7....",
    ".2..6..9.",
    "....1.8..",
    ".....943.",
    ".4.8..65.",
    "8.......1"]

-- http://act365.com/sudoku
rubylips1 = ["....4..3.", "98.6.1...", "......2..",
	"........1", "..4.5.7..", "6",
	"..5", "...9.8.76", ".7..3."]

rubylips2 = ["....3.", ".2.5.1", "4.....97.",
	"........5", "..8.7.6..", "3",
	".59.....1", "...6.4.3.", "....8...."]

-- 19 cells but easy
rubylips3 = [".....9", "....147", "..2",
	"7......86", "5...3...2", "94......1",
	"......4..", "..625....", "...8"]

-- 19 cells but hard
rubylips4 = [".2", "...6....3", ".74.8",
	".....3..2", ".8..4..1.", "6..5",
	"....1.78", "5....9", ".......4." ]

rubylips5 = [
    ".....97..",
    ".1....9..",
    "4...53.2.",
    ".....7...",
    ".3.....4.",
    "...8.....",
    ".6.41...2",
    "..9....5.",
    "..86....."]

rubylips6 = [
    "..96...5.",
    "6........",
    "3...21.8.",
    ".....5...",
    ".7.....2.",
    "...9.....",
    ".8.56...3",
    "........4",
    ".1...87.."]
	
rubylips7 = [
    "......6.3",
    "....4.2..",
    ".5.97...8",
    "...1.....",
    ".9.....4.",
    ".....3...",
    "1...94.7.",
    "..8.5....",
    "6.4......"]
	
rubylips8 = [
    "..8.4...5",
    "..63.....",
    "72.......",
    "5..1.....",
    "3.......4",
    ".....6..8",
    ".......13",
    ".....59..",
    "4...7.2.."]
	
rubylips9 = [
    ".......6.",
    "1..9.4...",
    ".3.7..8..",
    "..6...9..",
    ".8..3..7.",
    "..2...5..",
    "..9..2.4.",
    "...1.5..3",
    ".7......."]

rubylips10 = [
    ".......7.",
    "4...9.1..",
    "..36.2...",
    ".5....63.",
    "....8....",
    ".19....2.",
    "...4.35..",
    "..2.1...8",
    ".7......."]


-- www.sudokusolver.co.uk/grids_veryhard.html
solverVH1 = ["..8.6.4", ".3.....7.", "4..8.1..3",
	"..74.96", "", "..23.65", 
	"9..6.8..4", ".2.....9.", "..4.2.7.."]

