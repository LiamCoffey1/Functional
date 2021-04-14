%% -*- prolog -*-

%% PROLOG = PROgramming (in) LOGic

%% program = set of "axioms"

%% run a program = make "query", ask system to prove some statement
%%   PROLOG tries to prove that your query is true
%%     and in the process, does some computation

%% E.g. query:     grandparent(mary, X)
%%    yes, can show that this is true if: X=becky
%%    yes, .............................: X=bob
%%    yes, .............................: X=sue
%%    no

%% axiom: forall X, if man(X) then mortal(X).
%%   Prolog: capitalized identifier is "logic variable", lower case is "constant".
%% axiom: man(aristotle)

%% can conclude: mortal(aristotle)

%% . - functor (predicate name) acts like cons
%% . (a, b) == (a . b) in scheme
%% [] - empty list
%% [1,2,3] = .(1 .(2, .(3, [])))
%% [H|T] - a list with H matching head and T matching rest
%% [1,2,3] = [1| [2,3]]
%% [1,2| [3]] = [1,2,3| []]
%% _ - wildcard (dont care)
%% Given p([1,2,3,4])
%% ?- p([X|Y])
%% X = 1, Y = [2,3,4]
%% The query ([_,_,X|Y]
%% answers X=3, Y=[4]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  Arithmetics                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% biolt in predicates < > >= =< =

positive(N) :- N>0
non_zero(N) :- N<0 ; N>0.	       

%% operators + - * / sqrt exp cos
%% Note: Prolog is not imperitive
%% 1+4 just meant "the addition of 1 and 4
%% Consider
%% prime(2)
%% prime(1+1) would fail as cannot be unified
%% proper -> X is 1+1, prime(X)
%% N is E will succeed whenever N is unbound
%% and E is a arithmetic expression
| ?- X is sqrt(9),  Y is 2 ** 4,  Z is floor(3.14).
X = 3.0
Y = 16.0
Z = 3
N is 1+1.
N is 1+1, P is N*2, Q is P+Q.
N is X+1.
I is I+1.
I is 6, I is I+1.
I is 6, J is I+1.
%% All operations must be represented as relations
%% General: a function that takes k args in prolog k+1 args
minimum(X,Y,Z) is true if Z is the minimum of X and Y
minimum(X,Y,X) :- X<Y.
minimum(X,Y,Y) :- X>=Y.

%%                       Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% add one to a number
%% signum wich is x-1 if x>0 otherwise 0
%% max of two numbers
%% max of three numbers
%% abs val of number
	       
	       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 Recursion                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Towers of Hanoi
% transfer(N,A,B,I) is true if we can transfer N discs from A to B
  % using I as an intermediate peg.
  % Base case - 1 disc
  transfer(1,A,B,I) :- move(A,B).
  % Recursive case - N discs
  transfer(N,A,B,I) :-
    M is N-1, 
    transfer(M,A,I,B),  % Transfer topmost N-1 discs from A to I
    move(A,B),          % Move biggest disc from A to B
    transfer(M,I,B,A).  % Transfer remaining N-1 discs from I to B

%%                       Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fact
%% fib
%% ackermans
%% euclids gcd



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  Structures                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Suppose we want to represent cars with attributes make, age, price.

%% We might use a three-place structure called car; e.g. car(ford, 3, 5000)
%% might represent a 3-year-old Ford selling for $5,000.

%% Structures of this type could be used in clauses such as:

% has(P,C) is true if P has a car matching C
has(joe, car(ford,3,5000)).
has(joe, car(opel,2,6000)).
has(mick, car(toyota,5,1000)).
has(mick, car(ford,2,2000)).

%And we can pose queries like: "What kind of Ford does Mick have?"

% Query: has(mick, car(ford, Age, Price))
% Answer: Age=2, Price=2000

% If we only want to get information about some fields we can use Prolog's
%% "don't care" marker - the underscore character - to indicate this.

| ?- has(Person, car(ford,_,_)).
Person = joe ? ;
Person = mick

-> yes


%%If we wanted to know what make of car sold for under 5000, we might ask:

| ?- has(_, car(Make,_,Price)), Price < 5000.
Make = toyota
Price = 1000 ? ;
Make = ford
Price = 2000

-> yes


%%                Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Data on each employee of a company consists of the following: employee's name,
%% department in which s/he works, her/his position in the department
%% (secretary, head, accountant etc.), number of years of service, basic salary,
%% and the name of their immediate boss. The company director is his/her own boss!

%% Write a Prolog database containing the employees'
%% information (make up 5 or 6 entries) - this should be a list of facts
%% containing "employee-details" structures. Now, based on this, make up some
%% rules to answer the following: (the name of the rule, along with its arity
%% is given in each case)

%% department/2: Find the department in which some particular person works
%% manager/2: Given a person's name, find out who's the manager of the
%% department in which they work
%% valid_employee/1: Your list of facts should ideally form a tree; that is,
%% if we get a person's boss, and then their boss' boss and so on, we should
%% end up with the company director. Write a predicate which, when given a
%% person's name, will check if this is so.
%% basic_salary/2: Get a person's basic salary
%% real_salary/2: Get a person's real salary, by adding the information that:
%% All employees with over 5 years service get a bonus of $5,000
%% No employee (even after bonuses) can earn more than his/her boss
%% - use the "min" predicate here, and make sure to have a special case for
%% the director... 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  Recursive Structures                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%               Linked Lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% List of numbers
node(2, node(6, node(7, nil)))

% add_front(List,Elem,NewList) is true if NewList is List with Elem inserted at the beginning
add_front(List,Elem,NewList) :- NewList = node(Elem,List).

% add_back(List,Elem,NewList) is true if NewList is List with Elem inserted at the end

add_back(nil, Elem, NewList) :- 
  NewList = node(Elem,nil).   % New list with 1 element

add_back(node(Hd,Tl), Elem, NewList) :- 
  add_back(Tl, Elem, NewTl),  % Add Elem to the tail of the list
  NewList = node(Hd,NewTl).   % Answer is Hd along with the new tail


%%                Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get the first element in a list
%% get the last element in a list
%% sum all the elements in a list
%% add an element to a list in order
%% (that is, assuming the original list was ordered, the new one will still be ordered). 



%%                Binary Trees
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node(2, node(1,nil,nil), node(6, node(4,node(3,nil,nil), node(5,nil,nil)), node(7,nil,nil))
    
%%                Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Write a predicate tree_insert(Tree,Elem,NewTree) which is true if NewTree is the tree you get by adding the element
%% Elem to the tree Tree so as to preserve its ordering. Remember that there will now be three cases:
%%        If the tree is empty, add the element at the root
%%        If the tree isn't empty, and Elem is less than the element stored at the current node, then add Elem to the left subtree
%%        If the tree isn't empty, and Elem is greater than the element stored at the current node, then add Elem to the right subtree 
%%    Try running the following queries:
%%        tree_insert(nil,4,T1), tree_insert(T1,5,T2), tree_insert(T2,2,T3), tree_insert(T3,7,T4).
%%        tree_insert(nil,7,T1), tree_insert(T1,5,T2), tree_insert(T2,4,T3), tree_insert(T3,5,T4).
%%        tree_insert(nil,2,T1), tree_insert(T1,4,T2), tree_insert(T2,5,T3), tree_insert(T3,7,T4). 
%%    Notice how lop-sided the last tree is - clearly the structure of the tree depends
%%    on the sequence in which we insert its elements...
%% Write a predicate that calls write/1 for each element stored on the tree,
%% so that it prints out all elements in order
%% Write a predicate that gets the sum of all the elements on the tree
 %% Write a program that gets the height of the tree; i.e. the maximum length of any path from the root to a leaf.



    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  List Processing in Prolog                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% List operations - defined using rules and facts(non-procedural)
%% No exectuion order, instead you give recursive rules
%% and non recursive base cases that characterize tyhe operayion
%% you are defining
%% [john, mary, pat] - list with three elements
%% can be of any term
%% [] - empty list
%% [Hd | Tl] denotes a list whose head is Hd and tail is Tl
%% equiv = [john | [mary,pat]]
%% unification rules
%% [] only unifies []
%% [H1|T1] AND [H2|T2] only if H1 unifies with H2 and same for T
%% consequence?
%% [] can never be same as list of form [H|T]
%% size(List,N) is true if List has N elements
    
size([],0).
size([H|T],N) :- size(T,N1), N is N1+1.

% sumlist(List, N) is true if the elements of List sum to N
sumlist([],0).
sumlist([H|T],N) :- sumlist(T,N1), N is N1+H.
    
% contains(Elem, List) is true if List contains Elem
contains(X,[X|_]).
contains(X,[_|T]) :- contains(X,T).


%%                Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Write a predicate tree_insert(Tree,Elem,NewTree) which is true if NewTree is the tree you get by adding the element
%% Elem to the tree Tree so as to preserve its ordering. Remember that there will now be three cases:
%%        If the tree is empty, add the element at the root
%%        If the tree isn't empty, and Elem is less than the element stored at the current node, then add Elem to the left subtree
%%        If the tree isn't empty, and Elem is greater than the element stored at the current node, then add Elem to the right subtree 
%%    Try running the following queries:
%%        tree_insert(nil,4,T1), tree_insert(T1,5,T2), tree_insert(T2,2,T3), tree_insert(T3,7,T4).
%%        tree_insert(nil,7,T1), tree_insert(T1,5,T2), tree_insert(T2,4,T3), tree_insert(T3,5,T4).
%%        tree_insert(nil,2,T1), tree_insert(T1,4,T2), tree_insert(T2,5,T3), tree_insert(T3,7,T4). 
%%    Notice how lop-sided the last tree is - clearly the structure of the tree depends
%%    on the sequence in which we insert its elements...
%% Write a predicate that calls write/1 for each element stored on the tree,
%% so that it prints out all elements in order
%% Write a predicate that gets the sum of all the elements on the tree
 %% Write a program that gets the height of the tree; i.e. the maximum length of any path from the root to a leaf.



    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  List Processing in Prolog                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% List operations - defined using rules and facts(non-procedural)
%% No exectuion order, instead you give recursive rules
%% and non recursive base cases that characterize tyhe operayion
%% you are defining
%% [john, mary, pat] - list with three elements
%% can be of any term
%% [] - empty list
%% [Hd | Tl] denotes a list whose head is Hd and tail is Tl
%% equiv = [john | [mary,pat]]
%% unification rules
%% [] only unifies []
%% [H1|T1] AND [H2|T2] only if H1 unifies with H2 and same for T
%% consequence?
%% [] can never be same as list of form [H|T]
%% size(List,N) is true if List has N elements
    
size([],0).
size([H|T],N) :- size(T,N1), N is N1+1.

% sumlist(List, N) is true if the elements of List sum to N
sumlist([],0).
sumlist([H|T],N) :- sumlist(T,N1), N is N1+H.
    
% contains(Elem, List) is true if List contains Elem
contains(X,[X|_]).
contains(X,[_|T]) :- contains(X,T).

%%                Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% average(L,N) is true if N is the average of all the numbers in L, or just 0 if the sum is 0
%% sumpos(L,N) is true if N is the sum of all the positive numbers in L
%% sumsquare(L,N) is true if N is the sum of the squares of all the numbers in L
%% maxlist(L,N) is true if N is the largest element in the list L.
%% maxpos(L,N) is true if N is the position of the largest element in the list L. (If there's more than one occurrence of the maximum, then this should be the first position at which it appears.)
%%  final(L,E) is true if E is the final element in L
%%  evenpos(L) which prints out the elements of L at positions 2,4,6... up to the end of the list (Use write/1 to print out the elements.) 


%%%%%%%%%% Accumulators

collect_to(0,L) :- L=[].
collect_to(N,L) :- N>0, N1 is N-1, collect_to(N1,T), L=[N|T].

new_collect_to(0,[]).
new_collect_to(N,[N|T]) :- N>0, N1 is N-1, new_collect_to(N1,T).

%% Joining two lists - (same as append)
join_list([], L2, L2).
join_list([H1|T1], L2, [H1|L3]) :-  join_list(T1,L2,L3).

% bad_reverse(L1,L2) -  a bad implementation of list reversal
bad_reverse([],[]).
bad_reverse([H|T], L2) :-
    bad_reverse(T,NT), append(NT,[H],L2).
%% Too slow


%   myreverse(?List, ?Reversed)
%   is true when Reversed is has the same element as List but in a reversed 
%   order. List must be a proper list.

good_reverse(List, Reversed) :-
	good_reverse(List, [], Reversed).

good_reverse([], Reversed, Reversed).
good_reverse([Head|Tail], SoFar, Reversed) :-
    good_reverse(Tail, [Head|SoFar], Reversed).

%   pr_reverse(?List, ?Reversed)
%   is true when Reversed is has the same element as List but in a reversed 
%   order. List must be a proper list.

pr_reverse(List, Reversed) :-
	pr_reverse(List, [], Reversed).

pr_reverse([], Reversed, Reversed) :-
        format("\nInput=~q, Intermediate=~q, Output=~q",[[],Reversed,Reversed]).
pr_reverse([Head|Tail], SoFar, Reversed) :-
        format("\nInput=~q, Intermediate=~q, Output=~q",[[Head|Tail],SoFar,Reversed]),
	pr_reverse(Tail, [Head|SoFar], Reversed).

%% Format -- Print

%%                Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% - cutlast(L1,L2) which is true if L2 is L1 with the last element removed
%% - trim(L1,N,L2) which is true if L2 contains just the first N elements of L1
%% - evens(L1,L2) which is true if L2 contains just those elements in L1
%%   which are even in the same order 
%% - Use recursion and the last predicate to implement a predicate that sorts
%%   a list by iteratively moving the smallest element to the head, then the
%%   next smallest to the second position and so on.
%% - Write a predicate split(L1,N,L2,L3) which is true if L2 contains those
%%   elements of L1 less than or equal to N, and L3 contains those elements
%%   of L1 greater than N. (This is a lot like the ordered binary trees example.) 











%% Consider append:
append([], L, L)
append([H|T1], L2, [H|T3]) :-
   append(T1,L2,T3)
%% First fact - empty list appended to any list L gives L
%% Next - if you takea list that begins withHand has T1 as
%% the rest of the list and append it to alistLthen the resulting
%% appended listwill begin with H.
%% Moreover, the rest of the resultinglist,T3, is the result of appending
%% T1(the rest of the first list) withL2(thesecond input list).

append([1], [2,3], [1,2,3])
%% answers yes
%% because with H=1,T1=[], L2 =[2,3] and T3=[2,3]
%% it must be the case that append([],[2,3],[2,3])
%% is true and fact (1) says that this is so.

%% Inverting inputs and outputs
%% division is intentionally vague
%% It is often possible to “invert” a query and ask what inputs
%% would compute a given output.
append([1], X, [1,2,3])
%% This asks Prolog to find a list X such that
%% if we append[1] to X we willget[1,2,3].
%% answers -> [2,3]
%% how?
%% First Prolog tries to match the query against fact (1) or rule (2)
%% .Fact (1) doesn’t match (the first arguments differ) so we match rule(2).
%% This gives us H=1,T1=[],L2=X and T3 = [2,3].We next have to solve the body
%% of rule (2) which is append([],L2,[2,3]). Fact (1) matches this, and tells
%% us that L2=[2,3]=X, and that’s our answer


%% The member relation
member(X, [X|_])
member(X, [_|Y]):- member(X, Y)
%% This definition states that the first argument, X, is a member
%% of the second argument (a list) if X matches the head of the list or
%% if X is(recursively) a member of the rest ofthe list.
%% We dont have to tell prolog that x cant be a member of empty list
%% If we dont tell prolog something is true it is always false
%% membership in an empty list is impossible
%% We can use member to iterate through a list of values
%% If we want to know if any member ofa listL satisfies a predicatep,
%% we cansimply write:member(X,L),p(X).There is no explicit iteration
%% orsearching. We simply ask Prolog tofind anX such thatmember(X,L)
%% istrue (X is inL)andp(X) is true.Backtracking will find the “right”
%% value forX (if any suchX exists).This is sometimes called the “guess
%% and verify” technique.Thus we can querymember(X,[3,-3,0,10,-10]),
%% (X > 0).This asks for anX in the list[3,-3,0,10,-10] which is greater
%% than 0.422 Prolog answersX = 3 ;X = 10 ;Note too that our “obvious”
%% definition of member is not the onlyone possible.An alternative definition
%% (which is farless obvious) ismember(X,L) :-  append(_,[X|_],L).This definition
%% saysX is a member ofLif I can take some list (whose valueI don’t care about)
%% and append it to alist that begins withX (and whichends with values I don’t care
%% about)and get a list equal toL.Said more clearly,X is a member ofLifX is anywhere
%% in the “middle” ofL.423CS 538  Spring 2006©Prolog solves a query involvingmember
%% by partitioning the listL inall possible ways, and checking to seeifX ever is the
%% head of the secondlist. Thus formember(X,[1,2,3]),ittries the partition[]
%% and[1,2,3](exposing1as a possibleX), then[1]and[2,3] (exposing2) and finally[1,2]
%% and[3] (exposing3).


%% Sorting Algorithms
%% logic is apparent
%% stripped of details of data and control structures
%% Consider naive sort
%% Requires 2 things:
%% • The sorting is a permutation (areordering) of the values in L.
%% • The values are “in order” (ascendingor descending).
%% We can implement this concept of a sort directly in Prolog.
%% We
%% (a) permute an input list
%% (b) check if it is in sorted order
%% (c) repeat (a) & (b) until a sorting is found.


%% Permutations
perm(X, Y) will be true if list Y is a perm of list X
perm([], [])
%% empty list may only be permed to another empty list
perm(L, [H|T]) :-
append(V, [H|U], L),
append(V,U,W),
perm(W, T)
%% This rule says a list L may be permuted in to a list that begins with H
%% and ends with list T if:
%% (1) L may be partitioned into two lists, V and [H|U].
%%      (That is,H is somewhere in the “middle” of L).
%% (2) Lists V and U (all of L except H)     may be appended into listW.
%% (3) Lis tW may be permuted intoT.

%% Let’s seeperm in action:
%% | ?- perm([1,2,3],X).
%% X = [1,2,3] ;
%% X = [1,3,2] ;
%% X = [2,1,3] ;
%% X = [2,3,1] ;
%% X = [3,1,2] ;
%% X = [3,2,1] ;
%% no

%% We’ll trace how the first few answersare computed.
%% Note though thatallpermutations are generated, and withno
%% apparent data structures orcontrol structures.We start with
%% L=[1,2,3] andX=[H|T].
%% We first solveappend (V,[H|U],L),which simplifies to
%% append(V,[H|U],[1,2,3]).
%% One solution to this goal isV = [], H = 1, U = [2,3]
%% We next solveappend(V,U,W)whichsimplifies toappend([],[2,3],W).
%% The only solution for this isW=[2,3].
%% Finally, we solveperm(W,T), whichsimplifies toperm([2,3],T).
%% One solution to this isT=[2,3].
%% This gives us our first solution:[H|T]=[1,2,3].
%% To get our next solution webacktrack.
%% Where is the most recent place wemade a choice of how to solve a goal?
%% It was atperm([2,3],T). We choseT=[2,3], butT=[3,2]
%% is anothersolution. Using this solution, we getout next answer[H|T]=[1,3,2].
%% Let’s try one more. We backtrackagain.
%% No more solutions are possibleforperm([2,3],T),
%% so we backtrackto an earlier choice point.
%% Atappend(V,[H|U],[1,2,3])another solution isV=[1], H = 2, U = [3]
%% Using this binding, we solveappend(V,U,W) which simplifies to
%% append([1],[3],W).
%% The solution tothis must beW=[1,3].
%% We then solveperm(W,T) whichsimplifies toperm([1,3],T).
%% Onesolution to this isT=[1,3].
%% Thismakes our third solution for[H|T] =[2,1,3].
%% You can check out the other bindings that lead to the last three solutions


%% Permutation Sort
%% inOrder
inOrder([]).
inOrder([_]).
inOrder([A,B|T]) :-
    A =< B, InOrder([B|T]).
%% A null list and a list with one ele already sorted
%% In order if first two elements are in proper order (A=<B)
%% checks this then rest of list

% new naive sort
naiveSort(L1,L2) :-
    perm(L1,L2), inOrder(L2).
%% | ?- naiveSort([1,2,3], [3,2,1])
%% no
%% | ?- naiveSort([3,2,1], L)
%% L = [1,2,3]
%% no
%% | ?- naiveSort([3,88,2,1,6,77,-23,5], L)
%% L = [-23,1,2,3,5,6,7,77,88]
%% Sort of works but hopelessly inefficient
%% repeatedly shuffles input until it finds sorted




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 Case analysis and Cut                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Cut predicate - !

grade(Mark, first) :- Mark>=70.
grade(Mark, two_1) :- Mark<70, Mark>=63.
grade(Mark, two_2) :- Mark<63, Mark>=55.
grade(Mark, third) :- Mark<55, Mark>=50.
grade(Mark, pass)  :- Mark<50, Mark>=40.
grade(Mark, fail)  :- Mark<40.

%% Works but inefficient

grade(N,first) :- N>=70, ! .
grade(N,two_1) :- N>=63, ! .
grade(N,two_2) :- N>=55, ! .
grade(N,third) :- N>=50, ! .
grade(N,pass) :- N>=40, ! .
grade(N,fail) :- N<40.

%% Eliminate useless backtracking
grade(N,first) :- N>=70, ! .
grade(N,two_1) :- N>=63, ! .
grade(N,two_2) :- N>=55, ! .
grade(N,third) :- N>=50, ! .
grade(N,pass) :- N>=40, ! .
grade(N,fail) :- N<40.

%% effects of cut
%% 1. Any variables which are bound to values at this point cannot take
%%    on other values
%% 1. No other versions of predicates called before the cut will be considered
%% 3. No other subsequent versions of the predicate at the head of the
%%    current rule will be considered
%% 4. The cut always succeeds.

%%                Excercises
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. Consider the following program which is intended to define the third
%%   argument to be the maximum of the first two numeric arguments:

%%  max(X,Y,X) :- X >= Y, !.
%%  max(X,Y,Y).

%%  -Provide an appropriate query to show that this program is
%%    incorrect (try using all constant arguments)
%%  -Change the program so that it works correctly 

%% 2. Consider the following program which is supposed to insert its
%%    first argument, a number, into its second argument, a sorted list,
%%    giving the third argument (also a sorted list):

%%  insert(X,[H|T],[H|T1]) :- X>H, !, insert(X,T,T1).
%%  insert(X,L,[X|L]).

%%  - Provide an appropriate query to show that this program is incorrect
%%  - Change the program so that it works correctly 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 Control Features                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Use sparingly!!!!


%% --- Green cuts
%% These are cuts which are introduced simply to make the program more
%% efficient by eliminating what the programmer knows to be useless computations.
%% They do not remove any extra solutions! Running a program without green cuts
%% should still give the same answer, even though it may take a little
%% longer to do so. 
%% --- Red cuts
%% These cuts are introduced to make the program run in a different way;
%% they do this by eliminating some of the possibilities that might be considered.
%% Thus they change the logical meaning of the program.

%% Avoid red cuts !!!!

%% Negation as Failure
%% defining predicates in terms of negation of other preficates
%% Thus to say "q is true if p isn't", we might write:
%% q :- p, !, fail.
%% q.

%% Shorthand = \+ - q:- \+(p) = Q is true when P fails
different(X,Y) :- X=Y, !, fail.
different(X,Y).
%% satisfied if x and y cannot be unified

%% Incorrect usage?
home(X) :- \+(out(X)).
out(sue).
%% Prolog assumes has all relevent information

%% If then else
s :- p, !, q.
s :- r.
%% Shorthand
s :- p -> q ; r.


% add(Elem, List, NewList) is true if adding Elem to List is NewList
% Elem is not added if it's there already.
add(X,L1,L2) :- member(X,L1), !, L2 = L1.
add(X,L1,L2) :- L2 = [X|L1].

%% Using the if-then-else notation, we could simply write this as:

add(X,L1,L2) :- member(X,L1) -> L2 = L1 ; L2 = [X|L1].


%% Repeat
%% Generally used on right hand side
%%
..... :- repeat,
           ( "Stuff to be iterated" ),
           ( "Termination Condition" ),
           !.

main_loop :- repeat,              % Start of iteration
                 display_menu,        % Print out the menu
                 get_option(N),       % Get input from user
                 validate_option(N),  % Check that it's valid
                 process_option(N),   % Carry out appropriate action
               is_quit_option(N),   % Termination Condition
               !.                   % Don't go back on any of this!
