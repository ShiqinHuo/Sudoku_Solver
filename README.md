# Sudoku_Solver_in_Haskell - Pure Functional Programming Application
This repository implements some **human-like algorithms** to solve Sudoku in Haskell. **纯函数式数独解题器**

For details, read **[Report.pdf](https://github.com/ShiqinHuo/Sudoku_Solver_in_Haskell/blob/master/Report.pdf)**
#### **Algorithm Overview:**
  * Solving Algorithm 1: Depth-first Search (Brute Force/Backtracking) 
  * Solving Algorithm 2: Prioritize the Guessing Order (Extension 1)
  * Solving Algorithm 3: Constraint Propagation (Extension 2) 
    * Hidden Singles (Solver level 1 in [sudoku.hs](https://github.com/ShiqinHuo/Sudoku_Solver_in_Haskell/blob/master/Sudoku.hs))   
    * Prune Naked Pairs (Solver level 2 in [sudoku.hs](https://github.com/ShiqinHuo/Sudoku_Solver_in_Haskell/blob/master/Sudoku.hs)) 
    * Prune Naked Triples (Solver level 3 in [sudoku.hs](https://github.com/ShiqinHuo/Sudoku_Solver_in_Haskell/blob/master/Sudoku.hs))
  * Generating Algorithm: Las Vegas Algorithm (Extension 3)
#### **Reasoning:**
* **EXTENSION 1: Optimizing the guessing order**
  * We are required to find the block with fewest candidates to reduce redundancy
  * In my implementation, I use num of choice to represent the levels of difficulty to work out
    that single position by guessing. Obviously, if the num of choice equals to min_choice(>1), it will have least redundancy.
  * expandFirst filters the best position to fill first according to the number of possibilities, which is exactly what we want in the extension 1.
  * If you want to test expanding without optimization, replace 'expandFirst' in 'search' by 'expand Performs the expansion beginning with the smallest number of choices (>1) breaks up a matrix is broken into five pieces: mc = rows1 ++ [row1 ++ cs : row2] ++ rows2
* **Extension 2 : Constraint Propagation to prune the huge search space (3 levels implemented)**
  * **levels**
    * level_1: prune singles; level_2: prune naked pairs; level_3: prune naked triples
  * **Reasoning**
    * Obviously, if there is only one element in the Choices list, the only value must be the solution for that position. This means, other positions in the blocks related to this known position cannot choose that value, so we have to reduce this value from the choices list of all related positions (sharing row/col/box)
  * LEVEL 1 for Constraint Propagation is to "remove singles"
  * Solver - LEVEL 1 Prune the single choice: 
    * Prune the single choice (a value occurs only once for a block) minus will remove the second set from the first.
  * Solver - LEVEL 2 : Prune the naked pairs  
    * In a Block Choices, we find exactly two positions with the choices [2,3]. Then obviously, the value 2 and 3 can only occur in those two places. All other occurrences of the value 1 and 9 can eliminated eliminate other occurrences if there exist dup pairs
    * prop> reduce_samepair [[Just 2,Just 3, Just 4, Just 5], [Just 2,Just 3,Just 4],[Just 2,Just 3],[Just 4,Just 3,Just 5,Just 1],[Just 2,Just 3],[Just 2,Just 5]]
    * [[Just 4,Just 5],[Just 4],[Just 2,Just 3],[Just 4,Just 5,Just 1],[Just 2,Just 3],[Just 5]]
    * dup_pair = [Just 2, Just 3]
  * Solver - LEVEL 3 : Prune the naked triples
    * Resembles Solver - LEVEL 2
    * In a Block Choices, we find exactly 3 positions with the choices [2,3,9]. All other simultaneous occurrences of the value [2,3,9] in sharing blocks can eliminated
    * note: higher levels can also be constructed similarly but there is no sense to filter not frequent occasions eliminate other simultaneous occurrences if there exist 3 same triples

4 Sep 2017
