% ---------------------------------------------------------------------
%  ----- Informatics 2D - 2011/12 - Second Assignment - Planning -----
% ---------------------------------------------------------------------
%
% Write here you matriculation number (only - your name is not needed)
% Matriculation Number: s1006260
%
%
% ------------------------- Problem Instance --------------------------
% This file is a template for a problem instance: the definition of an
% initial state and of a goal. 

 debug(on).	% need additional debug information at runtime?



% --- Load domain definitions from an external file -------------------

:- [domain-task1].		% Replace with the domain for this problem




% --- Definition of the initial state ---------------------------------

box(a).
box(b).
box(c).
connected(loc1-1, loc1-2, n).
connected(loc1-2, loc1-3, n).
connected(loc1-3, loc1-4, n).
connected(loc2-1, loc2-2, n).
connected(loc2-2, loc2-3, n).
connected(loc2-3, loc2-4, n).
connected(loc3-1, loc3-2, n).
connected(loc3-2, loc3-3, n).
connected(loc1-2, loc1-1, s).
connected(loc1-3, loc1-2, s).
connected(loc1-4, loc1-3, s).
connected(loc2-2, loc2-1, s).
connected(loc2-3, loc2-2, s).
connected(loc2-4, loc2-3, s).
connected(loc3-2, loc3-1, s).
connected(loc3-3, loc3-2, s).
connected(loc1-1, loc2-1, e).
connected(loc2-1, loc3-1, e).
connected(loc1-2, loc2-2, e).
connected(loc2-2, loc3-2, e).
connected(loc1-3, loc2-3, e).
connected(loc2-3, loc3-3, e).
connected(loc1-4, loc2-4, e).
connected(loc2-1, loc1-1, w).
connected(loc3-1, loc2-1, w).
connected(loc2-2, loc1-2, w).
connected(loc3-2, loc2-2, w).
connected(loc2-3, loc1-3, w).
connected(loc3-3, loc2-3, w).
connected(loc2-4, loc1-4, w).
at(a,loc2-3,s0).
at(b,loc2-2,s0).
at(c,loc2-1,s0).
at(agent,loc3-2,s0).
empty(loc1-1,s0).
empty(loc1-2,s0).
empty(loc1-3,s0).
empty(loc1-4,s0).
empty(loc2-4,s0).
empty(loc3-1,s0).
empty(loc3-3,s0).





% --- Goal condition that the planner will try to reach ---------------

goal(S) :-	(at(a,loc1-1,S),at(b,loc1-2,S),at(c,loc1-3,S)).
goal(S) :-	(at(a,loc1-1,S),at(b,loc1-3,S),at(c,loc1-2,S)).
goal(S) :-	(at(a,loc1-2,S),at(b,loc1-1,S),at(c,loc1-3,S)).
goal(S) :-	(at(a,loc1-2,S),at(b,loc1-3,S),at(c,loc1-1,S)).
goal(S) :-	(at(a,loc1-3,S),at(b,loc1-1,S),at(c,loc1-2,S)).
goal(S) :-	(at(a,loc1-3,S),at(b,loc1-2,S),at(c,loc1-1,S)).	% fill in the goal definition




% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
