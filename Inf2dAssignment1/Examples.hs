-- Inf2d Assignment 1 2011-12 
-- PLEASE DO NOT SUBMIT THIS FILE

module Examples where

import Data.List
import Data.Maybe
import CSPframework

{-
Some examples of constraints and CSPs.

Petros Papapanagiotou
School of Informatics
University of Edinburgh
2012

-}

-- *******************************************
-- ** Examples of Relations and Constraints **
-- *******************************************

-- Binary constraint stating that two variables must be different.
vars_diff_constraint :: Var -> Var -> Constraint
vars_diff_constraint a b = CT (a ++ " /= " ++ b,[a,b],vars_diff)

-- Relation that ensures two variables are different.
-- It is satisfied if either variable is unassigned.
vars_diff :: Relation
vars_diff vars assignment = not $ vals_eq (lookup_var assignment a) (lookup_var assignment b)
  where a = head vars
        b = head $ tail vars



-- N-ary constraint stating that all variables in a list must have have distinct values.
all_diff_constraint :: [Var] -> Constraint
all_diff_constraint vs = CT ("All_Diff: " ++ (show vs),vs,all_diff)

-- Relation that ensures a list of variables are all different.
-- Ignores unassigned variables.
all_diff :: Relation
all_diff vs a = length l == length (nub l) 
  where l = filter isJust $ map (lookup_var a) vs



-- N-ary constraint stating that the sum of a list of variables must have a specific value.
sum_constraint :: [Var] -> Int -> Constraint
sum_constraint vs i = CT (showsum ++ " = " ++ (show i),vs,have_sum i)
  where showsum =  (\l -> foldl (\a b ->  a ++ " + " ++ b) (head l) (tail l)) $ vs

-- Relation that ensures the sum of the variables is equal to the given value.
-- It is satisfied if at least one of the variables is unassigned.
have_sum :: Int -> Relation
have_sum sum vars assignment
  | t = (foldl (+) 0 $ map fromJust l) == sum
  | otherwise = True
    where l = map (lookup_var assignment) vars
          t = all isJust l
                  
              
-- Binary constraint stating that the absolute value of the difference of two variables is equal to 1.
abs_diff_one_constraint :: Var -> Var -> Constraint
abs_diff_one_constraint a b = CT ("|" ++ a ++ " - " ++ b ++ "| = 1",[a,b],abs_diff_one)

-- Relation that ensures the absolute value of the difference of two variables is equal to 1.
-- Ignores unassigned variables.
abs_diff_one :: Relation
abs_diff_one vs a 
	| ax == Nothing = True
	| ay == Nothing = True
	| otherwise = abs ((fromJust ax) - (fromJust ay)) == 1
	where ax = lookup_var a $ head vs
	      ay = lookup_var a $ head $ tail vs


-- Binary constraint stating that the difference of two variables is equal to 1.
diff_one_constraint :: Var -> Var -> Constraint
diff_one_constraint a b = CT (a ++ " - " ++ b ++ " = 1",[a,b],diff_one)

-- Relation that ensures the difference of two variables is equal to 1.
-- Ignores unassigned variables.
diff_one :: Relation
diff_one vs a
        | ax == Nothing = True
        | ay == Nothing = True
        | otherwise = (fromJust ax) - (fromJust ay) == 1
        where ax = lookup_var a $ head vs
              ay = lookup_var a $ head $ tail vs

              
          
-- **********************
-- ** Examples of CSPs **
-- **********************
          
-- Example 1: Map of Australia
          
colours = [1..3]

aus_regions :: [Var]
aus_regions = ["WA","NSW","NT","Q","V","SA"]

aus_domains :: Domains
aus_domains = map (\r -> (r,colours)) aus_regions

aus_csp :: CSP
aus_csp = CSP ("Map of Australia",
           aus_domains, [
             (vars_diff_constraint "WA" "NT"),
             (vars_diff_constraint "NT" "Q"),
             (vars_diff_constraint "SA" "WA"),
             (vars_diff_constraint "SA" "Q"),
             (vars_diff_constraint "SA" "NT"),
             (vars_diff_constraint "Q"  "NSW"),
             (vars_diff_constraint "SA" "NSW"),
             (vars_diff_constraint "SA" "V"),
             (vars_diff_constraint "V"  "NSW")
             ])


-- Example 2: Map of Scotland

map_regions :: [Var]
map_regions = ["ARGYLL","ARRAN","LOTHIAN","BORDERS","CENTRAL","D&G","FIFE","GRAMPIAN","HIGHLAND","ISLAY","MULL","ORKNEY","SKYE","STRATHCLYDE","TAYSIDE"]

map_domains :: Domains
map_domains = map (\ r -> (r,colours)) map_regions

map_csp = CSP ("Map of Scotland",
           map_domains, [
             (vars_diff_constraint  "ORKNEY"      "HIGHLAND" ),
             (vars_diff_constraint  "SKYE"        "HIGHLAND" ),
             (vars_diff_constraint  "MULL"        "HIGHLAND" ),
             (vars_diff_constraint  "GRAMPIAN"    "HIGHLAND" ),
             (vars_diff_constraint  "TAYSIDE"     "HIGHLAND" ),
             (vars_diff_constraint  "ARGYLL"      "HIGHLAND" ),
             (vars_diff_constraint  "GRAMPIAN"    "TAYSIDE" ),
             (vars_diff_constraint  "MULL"        "ARGYLL" ),
             (vars_diff_constraint  "TAYSIDE"     "ARGYLL" ),
             (vars_diff_constraint  "CENTRAL"     "ARGYLL" ),
             (vars_diff_constraint  "ARRAN"       "ARGYLL" ),
             (vars_diff_constraint  "ISLAY"       "ARGYLL" ),
             (vars_diff_constraint  "STRATHCLYDE" "ARGYLL" ),
             (vars_diff_constraint  "MULL"        "ISLAY" ),
             (vars_diff_constraint  "FIFE"        "TAYSIDE" ),
             (vars_diff_constraint  "CENTRAL"     "TAYSIDE" ),
             (vars_diff_constraint  "ARRAN"       "STRATHCLYDE" ),
             (vars_diff_constraint  "FIFE"        "CENTRAL") ,
             (vars_diff_constraint  "FIFE"        "LOTHIAN" ),
             (vars_diff_constraint  "STRATHCLYDE" "CENTRAL" ),
             (vars_diff_constraint  "STRATHCLYDE" "LOTHIAN" ),
             (vars_diff_constraint  "STRATHCLYDE" "BORDERS" ),
             (vars_diff_constraint  "STRATHCLYDE" "D&G" ),
             (vars_diff_constraint  "BORDERS"     "D&G" ),
             (vars_diff_constraint  "BORDERS"     "LOTHIAN" )
             ])
          

-- Example 3: 3x3 Magic Square

ms3x3_domains :: Domains
ms3x3_domains = [("x1",[1..9]),("x2",[1..9]),("x3",[1..9]),("y1",[1..9]),("y2",[1..9]),("y3",[1..9]),("z1",[1..9]),("z2",[1..9]),("z3",[1..9])]

ms3x3_csp = CSP ("Magic Square 3x3",
                 ms3x3_domains,[
                   (all_diff_constraint (map fst ms3x3_domains)),
                   (sum_constraint ["x1", "x2", "x3"] 15), -- row 1
                   (sum_constraint ["y1", "y2", "y3"] 15), -- row 2
                   (sum_constraint ["z1", "z2", "z3"] 15), -- row 3
                   (sum_constraint ["x1", "y1", "z1"] 15), -- column 1
                   (sum_constraint ["x2", "y2", "z2"] 15), -- column 2
                   (sum_constraint ["x3", "y3", "z3"] 15), -- column 3
                   (sum_constraint ["x1", "y2", "z3"] 15), -- diagonal 1
                   (sum_constraint ["x3", "y2", "z1"] 15)  -- diagonal 2
                   ])

sudoku_vars :: [Var]
sudoku_vars = foldl union [] $ map (\x -> map (\y -> (++) x $ show y) [1..9]) ["a","b","c","d","e","f","g","h","i"]

sudoku_domains :: Domains
sudoku_domains = map (\r -> (r,[1..9])) sudoku_vars

sudoku_csp :: CSP
sudoku_csp = CSP ("Sudoku!",
           sudoku_domains, [
             (all_diff_constraint $ map (\y -> (++) "a" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "b" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "c" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "d" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "e" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "f" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "g" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "h" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "i" $ show y) [1..9]),
             
             (all_diff_constraint ["a1","b1","c1","d1","e1","f1","g1","h1","i1"]),
             (all_diff_constraint ["a2","b2","c2","d2","e2","f2","g2","h2","i2"]),
             (all_diff_constraint ["a3","b3","c3","d3","e3","f3","g3","h3","i3"]),
             (all_diff_constraint ["a4","b4","c4","d4","e4","f4","g4","h4","i4"]),
             (all_diff_constraint ["a5","b5","c5","d5","e5","f5","g5","h5","i5"]),
             (all_diff_constraint ["a6","b6","c6","d6","e6","f6","g6","h6","i6"]),
             (all_diff_constraint ["a7","b7","c7","d7","e7","f7","g7","h7","i7"]),
             (all_diff_constraint ["a8","b8","c8","d8","e8","f8","g8","h8","i8"]),
             (all_diff_constraint ["a9","b9","c9","d9","e9","f9","g9","h9","i9"]),
             
             (all_diff_constraint ["a1","a2","a3","b1","b2","b3","c1","c2","c3"]),
             (all_diff_constraint ["a4","a5","a6","b4","b5","b6","c4","c5","c6"]),
             (all_diff_constraint ["a7","a8","a9","b7","b8","b9","c7","c8","c9"]),
             (all_diff_constraint ["d1","d2","d3","e1","e2","e3","f1","f2","f3"]),
             (all_diff_constraint ["d4","d5","d6","e4","e5","e6","f4","f5","f6"]),
             (all_diff_constraint ["d7","d8","d9","e7","e8","e9","f7","f8","f9"]),
             (all_diff_constraint ["g1","g2","g3","h1","h2","h3","i1","i2","i3"]),
             (all_diff_constraint ["g4","g5","g6","h4","h5","h6","i4","i5","i6"]),
             (all_diff_constraint ["g7","g8","g9","h7","h8","h9","i7","i8","i9"])
             ])

sudoku vals = foldl (\c (x,y) -> set_domain c x [y]) sudoku_csp vals

sudoku1 = sudoku [("a3",3),("a5",2),("a7",6),
                  ("b1",9),("b4",3),("b6",5),("b9",1),
                  ("c3",1),("c4",8),("c6",6),("c7",4),
                  ("d3",8),("d4",1),("d6",2),("d7",9),
                  ("e1",7),("e9",8),
                  ("f3",6),("f4",7),("f6",8),("f7",2),
                  ("g3",2),("g4",6),("g6",9),("g7",5),
                  ("h1",8),("h4",2),("h6",3),("h9",9),
                  ("i3",5),("i5",1),("i7",3)
                  ]
