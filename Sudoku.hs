-- Name: ShiqinHuo
-- UID: u5949730
-- Collaborators: ShiqinHuo
{-# LANGUAGE TupleSections #-}
module Sudoku
  ( allBlanks
  , isSudoku
  , noBlanks
  , printSudoku
  , fromString
  , toString
  , rows
  , cols
  , boxs
  , okBlock
  , okSudoku
  , blank
  , (!!=)
  , update
  , solve
  ) where
import Test.QuickCheck
import Data.Maybe (fromJust, isNothing)
import Data.List ((\\), transpose, nub, isInfixOf)
import Data.Char (isDigit, digitToInt, intToDigit)
-- A matrix is a list of rows.
type Matrix a = [Row a]
-- A row is a list of values
type Row a = [a]
-- A Sudoku puzzle is a matrix of cells
newtype Sudoku =
  Sudoku (Matrix Cell)
  deriving (Show, Eq)
-- | cells extracts the matrix from a Sudoku
cells (Sudoku m) = m
-- Each cell may contain a number from 1 to 9, or nothing
type Cell = Maybe Int
example :: Sudoku
example =
  Sudoku
    [ [ Just 3
      , Just 6
      , Nothing
      , Nothing
      , Just 7
      , Just 1
      , Just 2
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 5
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 8
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 9
      , Just 2
      , Nothing
      , Just 4
      , Just 7
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 3
      , Nothing
      , Just 2
      , Just 8
      ]
    , [ Just 4
      , Nothing
      , Nothing
      , Just 5
      , Nothing
      , Just 2
      , Nothing
      , Nothing
      , Just 9
      ]
    , [ Just 2
      , Just 7
      , Nothing
      , Just 4
      , Just 6
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 5
      , Just 3
      , Nothing
      , Just 8
      , Just 9
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 8
      , Just 3
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 6
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 7
      , Just 6
      , Just 9
      , Nothing
      , Nothing
      , Just 4
      , Just 3
      ]
    ]
-- allBlanks is a Sudoku with just blanks
allBlanks :: Sudoku
allBlanks = Sudoku (replicate 9 (replicate 9 Nothing))
-- | isSudoku checks if a Sudoku has the proper dimensions
-- >>> isSudoku (Sudoku [])
-- False
-- >>> isSudoku allBlanks
-- True
-- >>> isSudoku example
-- True
-- >>> isSudoku (Sudoku (tail (cells example)))
-- False
-- | isSudoku sud checks if sud is really a valid representation of a sudoku
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku s)      = validRows && validCols && validNums
  where validRows        = length s == 9
        validCols        = and $ map ((== 9) . length) s
        validNums        = and $ concatMap (map isValid) s
        isValid Nothing  = True
        isValid (Just n) = n >= 1 && n <= 9
-- | noBlanks checks if a Sudoku has no blanks
noBlanks :: Sudoku -> Bool
noBlanks (Sudoku s) = (Nothing `notElem`) $ concat s
-- | printSudoku prints a Sudoku as a 9 x 9 grid
-- Example:
--    3 6 . . 7 1 2 . .
--    . 5 . . . . 1 8 .
--    . . 9 2 . 4 7 . .
--    . . . . 1 3 . 2 8
--    4 . . 5 . 2 . . 9
--    2 7 . 4 6 . . . .
--    . . 5 3 . 8 9 . .
--    . 8 3 . . . . 6 .
--    . . 7 6 9 . . 4 3
printSudoku :: Sudoku -> IO ()
printSudoku = mapM_ putStrLn . map (concatMap printCell) . cells
    where
        printCell Nothing = "."
        printCell (Just n) = show n
-- | cell generates an arbitrary cell in a Sudoku
-- The frequency of Nothing versus Just n values is currently 90% versus 10%,
-- but you may want to change that ratio.
cell :: Gen (Maybe Int)
cell = frequency
    [(10, oneof [return (Just n) | n <- [1 .. 9]]), (90, return Nothing)]
-- | An instance for generating Arbitrary Sudokus
-- prop> isSudoku s
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [sequence [cell | j <- [1 .. 9]] | i <- [1 .. 9]]
    return (Sudoku rows)
--(fromString toString) are based on https://github.com/Eriuo/Haskell/blob/master/Sudoku/Sudoku.hs
-- | fromString converts an 81-character canonical string encoding for a
-- | Sudoku into our internal representation
fromString :: String -> Sudoku
fromString xs = Sudoku (group xs)
-- | group a string into a nested list consisting of 9 lists
group :: String -> Matrix Cell
group xs = chunk 9 (concat [convert s | s <- ys])
    where
        ys = lines xs
chunk :: Int -> Row Cell -> Matrix Cell
chunk _ [] = []
chunk n l
    | n > 0 = (take n l) : (chunk n (drop n l))
    | otherwise = error "Negative n"
-- http://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
-- | converts a string into a row
convert :: String -> Row Cell
convert str = case str of
            [] -> []
            ('.':xs) -> [Nothing] ++ convert xs
            (x:xs)
                | isDigit x -> [Just (digitToInt x)] ++ convert xs
                | otherwise -> error "Not a sudoku!"
-- | toString converts a Sudoku into its canonical 81-character string
-- | encoding
-- prop> fromString (toString s) == s
toString :: Sudoku -> String
toString (Sudoku s) = concat $ [printRow (selectRow s x) | x <- [0..8]] -- traversal of all rows (x is the index)
    where
        printRow list = case list of  -- coverts a single row cell into a string
            []           -> ""
            (Nothing:xs) -> "." ++ printRow xs
            (Just a:xs)  -> [intToDigit a] ++ printRow xs
-- | the Int represents the row index in the matrix
selectRow :: Matrix Cell -> Int -> Row Cell
selectRow (x:_) 0  = x
selectRow (x:xs) n = selectRow xs (n-1)
type Block a = [a]
-- | rows cols boxs are blocks which have to satisfy 2 constraints : No blanks & No duplicates when finished
rows :: Matrix a -> [Block a]
rows = id
cols :: Matrix a -> [Block a]
cols = transpose
boxs :: Matrix a -> [Block a]
boxs = unpack . (map cols) . pack
    where
        pack = split . map split
        split = chop 3
        unpack = map concat . concat
-- Split the list in sublists of length N (the given int)
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
-- | Test if a block of cells does not contain the same integer twice
-- >>> okBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
-- True
-- >>> okBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
-- False
okBlock :: Block Cell -> Bool
okBlock b = nub (removeNothing b) == removeNothing b
-- Remove 'Nothing'
removeNothing :: Block Cell -> Block Cell
removeNothing b = case b of
    []           -> []
    (Nothing:xs) -> removeNothing xs
    (x:xs)       -> [x] ++ removeNothing xs
-- | No block contains the same integer twice
-- >>> okSudoku allBlanks
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- True
-- >>> toString $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"
-- True
-- As long as all the blocks are valid with no dup integer, the Sudoku meets the non-duplicate constrain.
okSudoku :: Sudoku -> Bool
okSudoku (Sudoku s) = all okBlock (boxs s) &&
                      all okBlock (rows s) &&
                      all okBlock (cols s)
-- | okSudoku constrains the duplicates
-- | noBlanks constrains the sudoku is finished without "Nothing" position
-- |  QuickCheck property to check whether the sudoku is valid
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku (Sudoku s) = isSudoku (Sudoku s)
type Pos = (Int, Int)
-- | Return a blank position in the Sudoku
-- >>> blank allBlanks
-- (0,0)
-- >>> blank example
-- (0,2)
blank :: Sudoku -> Pos
blank = head . allBlankPos
-- |Finds all blank spaces in a sudoku
indexedPuzzle :: Sudoku -> [(Int, [(Int, Maybe Int)])]
indexedPuzzle = zip [0..8] . map (zip [0..8]) . cells
allBlankPos :: Sudoku -> [Pos]
allBlankPos = toPosList . getBlanks . indexedPuzzle
    where getBlanksRow = map fst . filter (isNothing . snd)
          getBlanks    = map (fmap getBlanksRow)
          toPosList    = concatMap (\(r, xs) -> map (r,) xs)
-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]
-- The replace operator
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) l (i, x)
    | i < 0 || i > (length l)-1 = l
    | otherwise                 = (take i l) ++ [x] ++ (drop (i+1) l)
-- | Given a Sudoku, a position, and a new cell value,
-- | update the given Sudoku at the given position with the new value.
update :: Sudoku -> Pos -> Int -> Sudoku
update (Sudoku s) (r, c) i = Sudoku (take r s ++ [(s!!r) !!= (c, toCell i)] ++ drop (r+1) s)
    where
        toCell x = Just x
-- (!!=): After replacing an element, the length should be the same
prop_replace :: [a] -> (Int, a) -> Bool
prop_replace s (i,x) = length s == length (s !!= (i, x))
-- | Solver - level 1 : Brute Force / Backtracking
-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any
solve_level_0 :: String -> [String]
solve_level_0 str
    | not $ prop_Sudoku s = []
    | not $ okSudoku s    = []
    | noBlanks s          = [toString s]
    | otherwise           = concat solution_list
        where
            s = fromString str
            nextBlank = (head . allBlankPos) s
            solution_list = [solve_level_0 str | str <- str_list]
            str_list = [toString (update s nextBlank i) | i <- [1..9]]
-- Just leave this very slow solver here
-- If you'd like to test this slow solution, simply change the "solve" to "solve_level_0" in Main.hs
-- | Above is based on Tony's skeleton.
-- | Extensions begin here:
-- | Different approach to apply Constraint Propagation (does not use Tony's type signature for Propagate):
-- Instead, I use a new type Choices to represent the possibilities remaining for a single position of the 9*9 puzzle
-- First, we have to construct some function & types for preparation:
type Value = Maybe Int
type Choices = [Value]
values :: [Value]
values = [Just i | i <- [1..9] ]
empty :: Value -> Bool
empty = (== Nothing)
-- | Assigns all the possible choices to a Sudoku Puzzle
choices :: Matrix Value -> Matrix Choices
choices = map (map choice)
    where
        choice v = if empty v then values else [v]
-- | Reduce a matrix of choices to a choice of possible matrices
collapse :: Matrix [a] -> [Matrix a]
-- | A matrix of choices is represented as a choice of Matrix as the Cartesian product of a list of lists
-- [[1,2],[3,4],[5,6]] -> [[1,3,5],[1,3,6],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
-- helper function :
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (x:xs) = [y:ys | y <- x, ys <- cp xs]
-- | Implement collapse
collapse = cp . map cp
-- | Constraint Propagation (Extension 1) : to prune the huge search space
--   I implement 3 levels :
--   level_1: prune singles
--   level_2: prune naked pairs
--   level_3: prune naked triples
-- Obviously, if there is only one element in the Choices list, the only value must be the solution for that position
-- This means, other positions in the blocks related to this known position cannot choose that value, so we have to
-- reduce this value from the choices list of all related positions (sharing row/col/box)
-- LEVEL 1 for Constraint Propagation is to "remove singles"
-- | Solver - LEVEL 1 : Prune the single choice (a value occurs only once for a block)
-- | minus will remove the second set from the first:
minus :: Choices -> Choices -> Choices
xs `minus` ys = if is_single xs then xs else xs\\ys
-- | performs this removal on a Row of Choices.
reduce_single :: Block Choices -> Block Choices
reduce_single x = [xs `minus` singles | xs <- x ]
    where
        singles = concat (filter is_single x)
-- | remove any duplicate choice in any blocks(rows/cols/boxs) related
prune_level_1 :: Matrix Choices -> Matrix Choices
prune_level_1 = pruneBy boxs . pruneBy cols . pruneBy rows
    where
        pruneBy f = f . map reduce_single . f
-- | Solver - LEVEL 2 : Prune the naked pairs
-- | In a Matrix Choices, we find exactly two positions with the choices [2,3].
-- | Then obviously, the value 2 and 3 can only occur in those two places.
-- | All other occurrences of the value 1 and 9 can eliminated
-- | eliminate other occurrences if there exist dup pairs
reduce_samepair :: Block Choices -> Block Choices
reduce_samepair x = [if (isInfixOf dup_pair xs)&&(dup_pair/=xs) then xs\\dup_pair else xs | xs <- x]
    where
        dup_pair = to_samepair x
-- | similar to prune_level_1
prune_dup_pair :: Matrix Choices -> Matrix Choices
prune_dup_pair = pruneBy boxs . pruneBy cols . pruneBy rows
    where
        pruneBy f = f . map reduce_samepair . f
-- | combine 2 pruning levels
prune_level_2 :: Matrix Choices -> Matrix Choices
prune_level_2 x = prune_dup_pair (prune_level_1 x)
-- | Solver - LEVEL 3 : Prune the naked triples
-- | Resembles Solver - LEVEL 2
-- | In a Matrix Choices, we find exactly 3 positions with the choices [2,3,9].
-- | All other simultaneous occurrences of the value [2,3,9] in sharing blocks can eliminated
-- note: higher levels can also be constructed similarly but there is no sense to filter not frequent occasions
-- | eliminate other simultaneous occurrences if there exist 3 same triples
reduce_sametriple :: Block Choices -> Block Choices
reduce_sametriple x = [if (isInfixOf dup_triple xs)&&(dup_triple/=xs) then xs\\dup_triple else xs | xs <- x]
    where
        dup_triple = to_sametriple x -- [2,3,6]
-- | similar to prune_level_1 and prune_dup_pair
prune_dup_triple :: Matrix Choices -> Matrix Choices
prune_dup_triple = pruneBy boxs . pruneBy cols . pruneBy rows
    where
        pruneBy f = f . map reduce_sametriple . f
-- | combine 3 pruning levels
prune_level_3 :: Matrix Choices -> Matrix Choices
prune_level_3 x = prune_dup_triple (prune_dup_pair (prune_level_1 x))
-- | recursively applies the function f to x as long as x /= f(x)
-- | This term in math is a "fix point":
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = if x == x' then x else fixpoint f x'
    where
        x' = f x
-- helper functions:
is_single :: [a] -> Bool
is_single [x] = True
is_single _   = False
is_pairs :: [a] -> Bool
is_pairs l = length l == 2
is_triple :: [a] -> Bool
is_triple l = length l == 3
--prop> to_samepair [[2,3,4,5],[2,3,4],[2,3],[4,3,5,1],[5,3],[2,3],[2,5]]
-- [2,3]
to_samepair :: Eq a => [[a]] -> [a]
to_samepair x = concat $ getDups pairs
    where
        pairs =  filter is_pairs x
getDups :: Eq a => [a] -> [a] -- eg. [[2,3],[2,3,4]]
getDups x = x \\ nub x
--prop> to_sametriple [[2,3,4,5],[2,3,4],[2],[4,3,5,1],[5],[2,3,4],[2,3,4],[2,5,3,4,6]]
-- [2,3,4]
to_sametriple :: Eq a => [[a]] -> [a]
to_sametriple xs = concat $ nub $ filter (\x -> count x xs == 3) triples
    where
        triples = filter is_triple xs
count :: Eq a => [a] -> [[a]] -> Int
count y ys = length $ filter (==y) ys
----------------
-- | When solved, each position in the sudoku matrix has only one choice
complete :: Matrix Choices -> Bool
complete = all (all is_single)
-- | The Sudoku is unsolvable, if any position has no choice:
unsolvable :: Matrix Choices -> Bool
unsolvable = any (any null)
-- | No duplicate in any the single choices in any block
consistent :: Block Choices -> Bool
consistent = nodup . concat . filter is_single
-- | Check whether there is no duplicate
nodup :: Eq a => [a] -> Bool
nodup l = nub l == l
-- | Satisfy sudoku constrains
satisfy :: Matrix Choices -> Bool
satisfy m = all consistent (rows m) && all consistent (cols m) && all consistent (boxs m)
search :: Matrix Choices -> [Matrix Value]
search m
    | unsolvable m || not (satisfy m) = []
    | complete m = collapse m
    | otherwise = [s | m' <- expand m, s <- search (fixpoint prune_level_2 m')]
                                         -- change the number 1,2,3 to control different levels
                                         -- 'fromSudoku' function should also be changed as the same time
                                         -- replace 'expandFirst' by 'expand to test the naive expand
-- | Expand WITHOUT considering the guessing order
expand :: Matrix Choices -> [Matrix Choices]
expand m =[rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
        (rows1, row : rows2) = break (any(not . is_single)) m
        (row1, cs : row2) = break (not . is_single) row
-- if you want to test expanding without optimization, replace 'expandFirst' in 'search' by 'expand
-- | Optimizing the guessing order
-- | Performs the expansion beginning with the smallest number of choices (>1)
-- breaks up a matrix is broken into five pieces:
-- mc = rows1 ++ [row1 ++ cs : row2] ++ rows2
expandFirst :: Matrix Choices -> [Matrix Choices]
expandFirst mc  = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
        (rows1,row:rows2) = break (any best) mc
        (row1,cs:row2)    = break best row
        best cs           = (length cs == n)
        n                 = minchoice mc
        minchoice         = minimum . filter(>1) . concat . map(map length)
-- | Solve function : main purpose is to convert between types: Sudoku, Matrix and String.
-- | related to the use of list comprehension & nested list
solve :: String -> [String]
solve str = to_listOfStr list_Of_matrix
    where
        sud = fromString str
        list_Of_matrix = fromSudoku sud
        to_listOfStr list_Of_matrix = [ toString' m | m <- list_Of_matrix ]
-- helper functions (typical Top-down):
-- | converts Sudoku to choices matrix which is applied in pruning part
fromSudoku :: Sudoku -> [Matrix Value]
fromSudoku (Sudoku m) = search (prune_level_2 (choices m))
                                -- change the number 1,2,3 to control different levels
                                --'search' function should also be changed as the same time
-- | converts a matrix to string
toString' :: Matrix Cell -> String
toString' m = concat $ [printRow (selectRow m x) | x <- [0..8]] -- traversal of all rows (x is the index)
    where
        printRow list = case list of  -- coverts a single row cell into a string
            []           -> ""
            (Nothing:xs) -> "." ++ printRow xs
            (Just a:xs)  -> [intToDigit a] ++ printRow xs
-- | reference:
-- http://www.programming-algorithms.net/article/42521/Sudoku
-- http://norvig.com/sudoku.html -- constraint propagation
-- http://hodoku.sourceforge.net/en/tech_naked.php
-- https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples
-- https://wiki.haskell.org/Sudoku
-- http://mfukar.github.io/2015/12/11/haskell-xvi.html
--https://github.com/pkukulak/sudoku/blob/master/src/Sudoku.hs