-- Inf2d Assignment 1 2011-12 
-- Matriculation number: s1006260

module Inf2d where

import Data.List
import Data.Maybe
import CSPframework
import Examples

{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions 
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is: 27th of February 2012.

-- See the reference manual for more information on the datatypes and functions of the framework.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- Currying
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, foldl, filter, sortBy etc.

-}

-------------------------------------------------
-- (3) Sudoku problem
-------------------------------------------------

-- (3.i) Variables & values

houses = [1..5]
people = ["miss scarlet", "col. mustard", "rev. green", "mrs peacock", "prof plum"]
weapons = ["candlestick", "dagger", "lead pipe", "revolver", "rope"]
rooms = ["kitchen", "lounge", "billiard room", "library", "dining room"]
colour = ["green", "red", "blue", "yellow", "white"]
enterprise = ["accomplice", "innocent", "sleeping", "studying", "murderer"]


cluedo_vars :: [Var]
cluedo_vars = concat[people,weapons,rooms,colour,enterprise]


cluedo_domains :: Domains
cluedo_domains = map (\ r -> (r,houses)) cluedo_vars



-- (3.ii) Constraints


-- Unary constraint stating that a variable must have a specific value.
has_value_constraint :: Var -> Int -> Constraint
has_value_constraint v i = CT (v ++ " = " ++ (show i),[v],has_value i)

-- Relation that ensures a variable has a given value.
-- It is satisfied if the variable is unassigned.
has_value :: Int -> Relation
has_value b vars assignment = vals_eq2 (lookup_var assignment a) (Just b)
  where a = head vars

-- Binary constraint stating that two variables must be equal.
vars_same_constraint :: Var -> Var -> Constraint
vars_same_constraint a b = CT (a ++ " = " ++ b,[a,b],vars_same)

-- Relation that ensures two variables are the same.
-- It is satisfied if either variable is unassigned.
vals_eq2 :: Maybe Int -> Maybe Int -> Bool
vals_eq2 x y 
        | isNothing x = True
        | isNothing y = True
        | otherwise = x == y

vars_same :: Relation
vars_same vars assignment = vals_eq2 (lookup_var assignment a) (lookup_var assignment b)
  where a = head vars
        b = head $ tail vars


-- (3.iii) Cluedo CSP

cluedo_csp :: CSP
cluedo_csp = CSP ("Cluedo!",
             cluedo_domains, [ 
                   (vars_same_constraint "miss scarlet" "green"),
                   (vars_same_constraint "col. mustard" "innocent"),
                   (vars_same_constraint "red" "lounge"),
                   (vars_same_constraint "mrs peacock" "kitchen"),
                   (vars_same_constraint "dagger" "sleeping"),
                   (vars_same_constraint "lead pipe" "yellow"),
                   (vars_same_constraint "revolver" "library"),
                   (vars_same_constraint "rev. green" "candlestick"),
                   (has_value_constraint "prof plum" 1),
                   (has_value_constraint "billiards room" 3),
                   (diff_one_constraint "red" "blue"),
                   (abs_diff_one_constraint "accomplice" "rope"),
                   (abs_diff_one_constraint "lead pipe" "studying"),
                   (abs_diff_one_constraint "prof plum" "white"),
                   (all_diff_constraint weapons),
                   (all_diff_constraint people),
                   (all_diff_constraint rooms),
                   (all_diff_constraint colour),
                   (all_diff_constraint enterprise)
               ])
            


-------------------------------------------------
-- (4.1) Forward Checking
-------------------------------------------------


-- (4.1.i) Forward check for constraint propagation:

forwardcheck :: CSP -> Assignment -> Var -> (CSP, Bool)
forwardcheck csp assignment var = ((fh2 csp assignment (fh0 csp var) (fh3 csp var)), (is_consistent csp assignment))

-- function which finds all neighbours of a given variable. Unnessary really, but it was left there after changes.

fh0 :: CSP -> Var -> [Var]
fh0 csp var = all_neighbours_of csp var

-- function which recursively loops through a list of variables deleting from their domains the value of int if it inconsistent

fh1 :: CSP -> Assignment -> [Var] -> Int -> CSP
fh1 csp assignment (var:vars) int = if not (is_consistent_value csp assignment var int) then fh1 (del_domain_val csp var int) assignment vars int
	else fh1 csp assignment vars int
fh1 csp assignment [] int = csp

-- function which recursively loops through a list of intergers, calling fh1 with each integer.

fh2 :: CSP -> Assignment -> [Var] -> [Int] -> CSP
fh2 csp assignment vars (int:ints) = fh2 (fh1 csp assignment vars int) assignment vars ints
fh2 csp assignment vars [] = csp

-- function which returns a comain domain for the neighbours of a variable

fh3 :: CSP -> Var -> [Int]
fh3 csp var =  nub (concat (map (domain_of csp) (all_neighbours_of csp var)))


-- (4.1.ii) Algorithm: 

fc_recursion :: Assignment -> CSP -> (Maybe Assignment, Int)
fc_recursion assignment csp =
    if (is_complete csp assignment) then (Just assignment,0)
	else find_consistent_value $ domain_of csp var
      where var = get_unassigned_var csp assignment 
            find_consistent_value vals = 
              case vals of  -- recursion over the possible values 
                            -- instead of for-each loop
                []     -> (Nothing,0)
                val:vs -> 
                  if (is_consistent_value csp assignment var val && snd newcsp) 
                  then if (isNothing result) 
                       then (ret,nodes+nodes'+1)
                       else (result,nodes+1)
                  else (ret,nodes'+1)
                     where (result,nodes) = fc_recursion (assign assignment var val) $ fst newcsp       
                           (ret,nodes') = find_consistent_value vs
                           newcsp = forwardcheck csp (assign assignment var val) var


fc :: CSP -> (Maybe Assignment,Int)
fc csp = fc_recursion [] csp 


-------------------------------------------------
-- (4.2) Minimum Remaining Values (MRV)
-------------------------------------------------

-- (4.2.i) Sorting function for variables based on the MRV heuristic:

mrv_compare :: CSP -> Var -> Var -> Ordering
mrv_compare csp x y = if length(domain_of csp x) == length(domain_of csp y) then (EQ)
			else if length(domain_of csp x) > length(domain_of csp y) then (GT)
				else (LT)


-- (4.2.ii) Get next variable according to MRV for the FC algorithm:

get_mrv_variable :: CSP -> Assignment -> Var
get_mrv_variable csp assignment = head (filter (is_unassigned assignment) (sortBy (mrv_compare csp) (vars_of csp)))


-- (4.2.iii) FC + MRV algorithm

fc_mrv_recursion :: Assignment -> CSP -> (Maybe Assignment, Int)
fc_mrv_recursion assignment csp =
    if (is_complete csp assignment) then (Just assignment,0)
	else find_consistent_value $ domain_of csp var
      where var = get_mrv_variable csp assignment 
            find_consistent_value vals = 
              case vals of  -- recursion over the possible values 
                            -- instead of for-each loop
                []     -> (Nothing,0)
                val:vs -> 
                  if (is_consistent_value csp assignment var val && snd newcsp) 
                  then if (isNothing result) 
                       then (ret,nodes+nodes'+1)
                       else (result,nodes+1)
                  else (ret,nodes'+1)
                     where (result,nodes) = fc_mrv_recursion (assign assignment var val) $ fst newcsp       
                           (ret,nodes') = find_consistent_value vs
                           newcsp = forwardcheck csp (assign assignment var val) var


fc_mrv :: CSP -> (Maybe Assignment,Int)
fc_mrv csp = fc_mrv_recursion [] csp 


-------------------------------------------------
-- (4.3) Least Constraining Value (LCV)
-------------------------------------------------

-- (4.3.i) Function that returns the number of choices available for all neighbours
--         of a variable:

num_choices :: CSP -> Assignment -> Var -> Int
num_choices csp assignment x = length(concat (map (domain_of newcsp) (nub(all_neighbours_of csp x))))
	where newcsp = fst (forwardcheck csp assignment x)


-- (4.3.ii) Function that sorts the domain of a variable based on the LCV heuristic.

lcv_sort :: CSP -> Assignment -> Var -> [Int]
lcv_sort csp assignment var = lh1 (lh0 csp assignment var)

-- function which produces a list of pairs of ints, the first of which represents the number of choices for 
-- a variable taking the value x and the second of which represents the value x

lh0 :: CSP -> Assignment -> Var -> [(Int,Int)]
lh0 csp assignment var = [(num_choices csp (assign assignment var x) var, x)| x <- (domain_of csp var)]

-- function which, given a list of pairs of ints, sorts it low to high by the first in each pair, reverses the order, and
-- then taking the second element of each pair to build a new list

lh1 :: [(Int,Int)] -> [Int]
lh1 xys = [snd (x,y) | (x,y) <- reverse(sort(xys))]

-- (4.3.iii) FC + LCV algorithm

fc_lcv_recursion :: Assignment -> CSP -> (Maybe Assignment, Int)
fc_lcv_recursion assignment csp =
    if (is_complete csp assignment) then (Just assignment,0)
	else find_consistent_value $ lcv_sort csp assignment var
      where var = get_unassigned_var csp assignment 
            find_consistent_value vals = 
              case vals of  -- recursion over the possible values 
                            -- instead of for-each loop
                []     -> (Nothing,0)
                val:vs -> 
                  if (is_consistent_value csp assignment var val && snd newcsp) 
                  then if (isNothing result) 
                       then (ret,nodes+nodes'+1)
                       else (result,nodes+1)
                  else (ret,nodes'+1)
                     where (result,nodes) = fc_lcv_recursion (assign assignment var val) $ fst newcsp       
                           (ret,nodes') = find_consistent_value vs
                           newcsp = forwardcheck csp (assign assignment var val) var



fc_lcv :: CSP -> (Maybe Assignment,Int)
fc_lcv csp = fc_lcv_recursion [] csp 


-- (4.3.iv) FC + MRV + LCV algorithm

fc_mrv_lcv_recursion :: Assignment -> CSP -> (Maybe Assignment, Int)
fc_mrv_lcv_recursion assignment csp =
    if (is_complete csp assignment) then (Just assignment,0)
	else find_consistent_value $ lcv_sort csp assignment var
      where var = get_mrv_variable csp assignment 
            find_consistent_value vals = 
              case vals of  -- recursion over the possible values 
                            -- instead of for-each loop
                []     -> (Nothing,0)
                val:vs -> 
                  if (is_consistent_value csp assignment var val && snd newcsp) 
                  then if (isNothing result) 
                       then (ret,nodes+nodes'+1)
                       else (result,nodes+1)
                  else (ret,nodes'+1)
                     where (result,nodes) = fc_mrv_lcv_recursion (assign assignment var val) $ fst newcsp       
                           (ret,nodes') = find_consistent_value vs
                           newcsp = forwardcheck csp (assign assignment var val) var


fc_mrv_lcv :: CSP -> (Maybe Assignment,Int)
fc_mrv_lcv csp = fc_mrv_lcv_recursion [] csp




-------------------------------------------------
-- (5) Evaluation
-------------------------------------------------
{-
  (5.ii) Table:

----------+--------+--------+--------+---------+----------+---------+-----------+
          |   BT   |   FC   | FC+MRV | FC+LCV  |FC+MRV+LCV|   AC3   |AC3+MRV+LCV|
----------+--------+--------+--------+---------+----------+---------+-----------+
aus_csp   |   37   |   15   |   06   |   15    |    06    |         |           |
map_csp   |  1176  |   398  |   15   |   398   |    15    |         |           |
ms3x3_csp |  3654  |   410  |   110  |   388   |    106   |xxxxxxxxx|xxxxxxxxxxx|
cluedo_csp|  45905 |  9191  |   60   |  1672   |    47    |         |           |
----------+--------+--------+--------+---------+----------+---------+-----------+

  (5.iii - 5.iv) Report:

- Your report must be no longer than one A4 page for the first four algorithms
  and another maximum half page for AC3 and AC3+MRV+LCV.
- Write your report starting here. -
-------------------------------------------------
The values in the table show the number of nodes visted by each algorithm for each csp. Forward checking, in each case,			fc vs bt	
drastically reduces the number of nodes visited when compared to simple Backtracking. This is definitely what I would					
have expected, as forward checking dramatically reduces the sizes of the demains of each variable, and thus the amount					
of nodes it is necessary to visit.

The way in which I implemented my forward checking algorithm, however, is far from perfect, as although it does dramatically		improving fc
reduce the number of nodes searched, the runtime is rather slow. This is likely	due to how I implemented the checking of whether
values were arc consistent - for each neighbour variable I checked all values domain of ANY neighbour variable. It would likely 
reduce runtime if the algorithm only checked values in the domain of the variable it was looking at, especially when considering
larger csps with much larger  potential domains.

Next came the implementation of some heuristics, namely MRV and LCV. MRV, in every case, again dramatically reduces the			mrv vs fc	
number of nodes visited when compared to FC. This is hardly surprising, as the MRV heuristic reduces the number of searches				
through variables by immediatly eliminating variables with no legal values. LCV, on the other hand, did not always reduce the		lcv vs fc	
number of nodes searched - in the cases of both the aus and map csps it made no difference at all. This surprised me, initially				
as I had expected the number of nodes to decrease. After some thought, however, I realised that LCV was not going to affect				
these csps as the variables only had domains containing 3 integers. Once 1 integer had been assigned, there was freedom of				
choice between the other two for any neighbours (hence LCV would not apply), and when 2 had been assigned there would be no				
choice at all. This also motivated another observation - that LCV did not reduce the number of nodes by as much as MRV did.		lcv vs mrv	
I realised that this was due to the fact that there were more variables (which are 'ordered' by MRV) than values the domains				
of the variables could take (which are ordered by LCV), hence MRV would clearly have much more of an impact. On different problems			
with larger domains LCV could have a much bigger impact than is demonstrated here, however for these csps MRV is the more				
effective heuristic.																	
																			
Finally there was the the combined MRV/LVC algorithm. Compared to MRV it didn't improve very much (obviously it didn't improve		mrv vs mrv+lcv	
at all in the map csps), due to the same reasons as before, namely, the small domains of the csps. It still mildly improved				
matters though, if only by a few nodes, and applied to larger scale csps would likely make a much larger difference, but on				
small csps it seems slighly uneccessary.														
																			
-------------------------------------------------
- End of report-
-}


-------------------------------------------------
-- (6) Arc Consistency
-------------------------------------------------


-- (6.i) Checks if there exists at least one value in a list of values that if 
-- assigned to the given variable the assignment will be consistent.

exists_consistent_value :: CSP -> Var -> Int -> Var -> [Int] -> Bool
exists_consistent_value csp x val y ys = or[check_constraints (common_constraints csp x y) (assign (assign [] x val) y z)| z <- ys]

-- (6.ii) AC-3 constraint propagation

revise :: CSP -> Var -> Var -> (CSP,Bool)
revise csp x y = (newcsp, (domain_of newcsp x < domain_of csp x))
	where newcsp = rh0 csp x y (domain_of csp x)  

-- Funtion that takes 2 variables and a list of integers as arguments, and for each integer checks whether there exists a consistent value
-- in the domain of y for that integer. If there is not, the integer is deleted from the domain of x

rh0 :: CSP -> Var -> Var -> [Int] -> CSP
rh0 csp x y [] = csp
rh0 csp x y (int:ints) = if not(exists_consistent_value csp x int y (domain_of csp y)) then rh0 (del_domain_val csp x int) x y ints
			else rh0 csp x y ints

-- (6.iii) AC-3 constraint propagation
        
ac3_check :: CSP -> [(Var,Var)] -> (CSP,Bool)
ac3_check csp [] = (csp, True)
ac3_check csp (arc:arcs) = if (fst arc) == (snd arc) then ac3_check csp arcs				
				else if snd (revise csp (fst arc) (snd arc)) then
					if length (domain_of csp (fst arc)) == 0 then (csp, False)
					else ac3_check csp (concat [arcs, [(y,(fst arc)) | y <- all_neighbours_of csp (fst arc)]])
				else ac3_check csp arcs


-- (6.iv) AC-3 algorithm

ac3_recursion :: Assignment -> CSP -> (Maybe Assignment, Int)
ac3_recursion assignment csp =
    if (is_complete csp assignment) then (Just assignment,0)
    else find_consistent_value $ domain_of csp var
      where var = get_unassigned_var csp assignment 
            find_consistent_value vals = 
              case vals of  -- recursion over the possible values 
                            -- instead of for-each loop
                []     -> (Nothing,0)
                val:vs -> 
                  if (snd (ac3_check csp ([(x,y) | x <- vars_of csp, y <- all_neighbours_of csp x]))) 
                  then if (isNothing result) 
                       then (ret,nodes+nodes'+1)
                       else (result,nodes+1)
                  else (ret,nodes'+1)
                     where (result,nodes) = ac3_recursion assignment (fst (ac3_check (newcsp) [(x,y) | x <- vars_of csp, y <- all_neighbours_of csp x]))
                           (ret,nodes') = find_consistent_value vs
			   newcsp = set_domain csp var [val]


ac3 :: CSP -> (Maybe Assignment,Int)
ac3 csp = ac3_recursion [] csp 



-- (6.v) AC-3 algorithm + MRV heuristic + LCV heuristic

ac3_mrv_lcv_recursion :: Assignment -> CSP -> (Maybe Assignment, Int)
ac3_mrv_lcv_recursion = undefined

 
ac3_mrv_lcv :: CSP -> (Maybe Assignment,Int)
ac3_mrv_lcv csp = ac3_mrv_lcv_recursion [] csp 

-------------------------------------------------
