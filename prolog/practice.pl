%% add one to a number
addOne(X, Y) :- Y is X + 1.
%% signum wich is x-1 if x>0 otherwise 0
signum(X,Y) :- X > 0, Y is X-1.
signum(X,0) :- X =< 0.
%% max of two numbers
max(X,Y,X) :- X > Y.
max(X,Y,Y) :- Y > X.
%% max of three numbers
max3(X,Y,Z,X) :- X > Y, X > Z.
max3(X,Y,Z,Y) :- Y > X, Y > Z.
max3(X,Y,Z,Z) :- Z > X, Z > Y.
%% abs val of number
abs(X, Y) :- X > 0 -> Y is X ; Y is X - (X * 2).


fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        fib(N1, F1),
        fib(N2, F2),
        F is F1+F2.

factorial(0, 1).
factorial(X, Y) :-
    X > 0,
    I is X - 1,
    factorial(I, R),
    Y is R * X.

gcd(A,B,X):- A=0,X=B. % base case
gcd(A,B,X):- B=0,X=A. % base case
gcd(A,B,X):- A>B, gcd(B, A, X).
gcd(A,B,X):- A<B, T is B mod A, gcd(A, T, X).
    

%% Data on each employee of a company consists of the following: employee's name,
%% department in which s/he works, her/his position in the department
%% (secretary, head, accountant etc.), number of years of service, basic salary,
%% and the name of their immediate boss. The company director is his/her own boss!

%% Write a Prolog database containing the employees'
%% information (make up 5 or 6 entries) - this should be a list of facts
%% containing "employee-details" structures. Now, based on this, make up some
%% rules to answer the following: (the name of the rule, along with its arity
%% is given in each case)

%% department\2: Find the department in which some particular person works
%% manager\2: Given a person's name, find out who's the manager of the
%% department in which they work
%% valid_employee (1): Your list of facts should ideally form a tree; that is,
%% if we get a person's boss, and then their boss' boss and so on, we should
%% end up with the company director. Write a predicate which, when given a
%% person's name, will check if this is so.
%% basic_salary\2: Get a person's basic salary
%% real_salary\2: Get a person's real salary, by adding the information that:
%% All employees with over 5 years service get a bonus of $5,000
%% No employee (even after bonuses) can earn more than his/her boss
%% - use the "min" predicate here, and make sure to have a special case for
%% the director...
employee("Liam", "Finance", "head", 22, 70000, "Liam").
employee("Joe", "Finance", "secretary", 4, 30000, "Liam").
employee("George", "Finance", "secretary", 5, 30000, "Liam").
employee("Adam", "Finance", "accountant", 3, 20000, "Liam").

%% deli_manager(First,Last) :- 
%%     department(_Deptno,'Deli',MgrID),
%%     employee(MgrID,Last,First,_Addr,_City,_St,_Zip,_Dept,_Date,_Sal).

department(Name, Department) :-
    employee(Name, Area, _Pos, _Yrs, _Amount, _Owner),
    Area = Department.

manager(Name, Manager) :-
    employee(Name, _Area, _Pos, _Yrs, _Amount, Owner),
    Owner = Manager.

%% real_salary(Name, Salary) :-
%%     employee(Name, _Area, _Pos, Yrs, Amount, _Owner),
%%     Yrs > 5000 -> Amount is Salary - 5000 ; Salary = Amount.


%% Linked lists
add_front(List,Elem,NewList) :- NewList = node(Elem,List).
add_back(nil, Elem, NewList) :- 
  NewList = node(Elem,nil).   % New list with 1 element
add_back(node(Hd,Tl), Elem, NewList) :- 
  add_back(Tl, Elem, NewTl),  % Add Elem to the tail of the list
  NewList = node(Hd,NewTl).   % Answer is Hd along with the new tail
getFirst(node(H, _), R) :- R = H.
getLast(node(H, nil), R) :- R = H.
getLast(node(_, T), R) :- getLast(T, R).
sumElements(node(H, nil), A, R) :- F is H + A, F = R.
sumElements(node(H, T), A, R) :- ACC is H + A, sumElements(T, ACC, R).

%% add an element to a list in order
%% (that is, assuming the original list was ordered, the new one will still be ordered).


%% Trees
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


%% node(2, node(1,nil,nil), node(6, node(4,node(3,nil,nil), node(5,nil,nil)), node(7,nil,nil))

%% Lists


evenpos([], _).
evenpos([H|T], I) :-
    (EVEN is I mod 2, EVEN = 0) ->
	(IN is I + 1),
	print(H),
	evenpos(T, IN) ;
    (IN is I + 1),
    evenpos(T, IN).

size([],0).
size([H|T],N) :- size(T,N1), N is N1+1.

avg(List,R) :-
    sumlist(List,Sum),
    size(List, Size),
    Size > 0,
    X is Sum / Size,
    R = X.

sumlist([],0).
sumlist([H|T],N) :- sumlist(T,N1), N is N1+H.

sumpos([], 0).
sumpos([H|T], R) :-
    (H >= 0) ->
	sumpos(T, N1), R is N1 + H ;
    sumpos(T, R).

sumsquares([], 0).
sumsquares([H|T], N) :- sumsquares(T, N1), N is N1 + (H ** 2).

final([H|[]], R) :- H = R.
final([H|T], R) :- final(T, R).

maxList([], MAX, R) :- MAX = R.
maxList([H|T], MAX, R) :- (H > MAX) -> maxList(T, H, R) ; maxList(T, MAX, R).

maxPos([], MAX, I, MAXI, R) :- MAXI = R.
maxPos([H|T], MAX, I, MAXI, R) :-
    (H > MAX),
    IN is I + 1 ->  maxPos(T, H, IN, IN, R) ;
    maxPos(T, MAX, IN, MAXI, R).


%% Accumulators

%% - cutlast(L1,L2) which is true if L2 is L1 with the last element removed
cutLast([H1|[]], [H2|[]]). 
cutLast([H1|T1], [H2|T2]) :-
    (H1 = H2),
    cutLast(T1, T2).

%% - trim(L1,N,L2) which is true if L2 contains just the first N elements of L1
trim(_, 0, _).
trim([H1|T1], N, [H2|T2]) :-
    H1 = H2,
    I is N-1,
    trim(T1, I, T2).


index(Matrix, Row, Col, Value):-
  nth1(Row, Matrix, MatrixRow),
  nth1(Col, MatrixRow, Value).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%               HW4                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%intersperse(a,[b,c,d,e],[b,a,c,a,d,a,e]).
%%intersperse(a,[x,y],[x,b,y]).
%%intersperse(a,[z],[z]).
%%intersperse(q,[],[]).

intersperse(C, L1, L2) :- inter(C, L1, L2, 0).
inter(_, [], [], _).
inter(X, [H1|T1], [H2|T2], I) :-
    (M1 is I mod 2),
    (M1 = 0) -> %% If even indexed
	(H1 = H2),
	(IT is I+1),
	inter(X, T1, T2, IT) ; %% else
    (IT is I+1),
    (H2 = X),
    inter(X, [H1|T1], T2, IT). %% Only advance ZS

take([_|_], 0, []).
take([H|_],1,[H]).
take([H|T1],N,[H|T2]):-
    N>=0,
    N1 is N-1,
    take(T1,N1,T2).

triangleify(List, Out) :-
    length(List, Len),
    triangle(List, 0, Len, [], Out).
triangle([], L, L, ACC, OUT) :- ACC = OUT.
triangle([C|N], I, L, ACC, OUT) :-
    INT is L - I,
    INC is I + 1,
    take(C, INT, SUB),
    append(ACC, [SUB], NEW),
    triangle(N, INC, L, NEW, OUT).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%% index from # of [n] .. 0




%% - evens(L1,L2) which is true if L2 contains just those elements in L1
%%   which are even in the same order

evens([], []).
evens([], [H2|[]]).
evens([H1|T1], [H2|T2]) :-
    (M1 is H1 mod 2),
    (M2 is H2 mod 2),
    (M1 = 0) ->
	(H1 = H2),
	evens(T1, T2) ;
    evens(T1, T2).
		       

valueAt([H|T], I, C, R) :-
    (not(C = I)) -> IT is C + 1, valueAt(T, I, C, R) ;
    valueAt([], I, C, H).
    

%% - Use recursion and the last predicate to implement a predicate that sorts
%%   a list by iteratively moving the smallest element to the head, then the
%%   next smallest to the second position and so on.


%% - Write a predicate split(L1,N,L2,L3) which is true if L2 contains those
%%   elements of L1 less than or equal to N, and L3 contains those elements
%%   of L1 greater than N. (This is a lot like the ordered binary trees example.)





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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exam Qs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
%% Q3: Prolog
%%  Define a Prolog predicate mul/3 (+,+,-) which is true when, given
%%  three lists, the length of the third is the product of the lengths
%%  of the first two.
%%  Examples:
%%  ?- mul([a,b],[a,b,c],[w,x,y,z,p,d])
%%  yes
%%  ?- mul([a,b],[b,c],Xs)
%%  Xs = [_,_,_,_,_,_]

mul(L1, L2, L3) :-
    length(L1, R1),
    length(L2, R2),
    length(L3, R3),
    SUM is R1 * R2,
    SUM = R3.


%% Q3: Prolog [25 marks]
%% Define a Prolog predicate thrice/2 which is true when its first argument appears three
%% times in its second argument, which must be a list.
%% Examples:
%% ?- thrice(e,[t,h,e,b,e,a,t,b,e]).
%% yes
%% ?- thrice(e,[t,h,e,b,e,a,t,b]).
%% no


thrice(_, _, 3) :- !.
thrice(E, [H|T], N) :-
    H = E ->
	INC is N + 1,
	thrice(E, T, INC) ;
    thrice(E, T, N).

%% Prolog
%% Define a Prolog predicate path(X,Y,G), where path(-,-,+), which
%% is true when there is a path from node X to node Y in a directed
%% graph G, where the graph is represented by a list of edges, each
%% represented by a two-element list of the source and destination
%% nodes.
%% (continued next page) 
%% CS424 Page 2 of 2 January 2017
%% Examples:
%%  ?- path(b,Y,[[a,b],[b,c],[b,d],[d,e]]).
%%  Y = c ;
%%  Y = d ;
%%  Y = e ;
%%  no
%%  ?- path(X,b,[[a,b],[b,c],[b,d],[d,e]]).
%%  X = a ;
%%  no
%%  ?- path(c,e,[[a,b],[b,c],[b,d],[d,e]]).
%%  yes
%% a-> b b -> c, b -> d, d->e
%% a -> b
%% b -> c
%% b -> d
%% d -> e

%% a -> b -> c
%% a -> b -> d
%% a -> b -> d -> e
%% path c -> e

%%   a b c d e
%% a 0 1 0 0 0
%% b 0 0 1 1 0
%% c 0 0 0 0 0
%% d 0 0 0 0 1
%% e 0 0 0 0 0

path(X, Y, [H|[X, Y]]).
path(X, Y, [[X, Y]|T]).
path(X, Y, [H|T]) :- path(X, Y, T).
    


%% 4. Define a Prolog predicate betwixt/3(+,-,+) which is true when
%% all three arguments are lists and the second argument is at least as
%% long as the first argument and no longer than the third argument.
%% Example:
%% | ?- betwixt([a,b],X,[c,d,e,f]).
%% X = [_,_] ? ;
%% X = [_,_,_] ? ;
%% X = [_,_,_,_] ? ;

betwixt(L1, L2, L3) :-
    length(L1, R1),
    length(L2, R2),
    length(L3, R3),
    R2 >= R1,
    R2 =< R3.


%% Define the Prolog predicate runs/1 which is true of lists whose
%% elements each have an identical adjacent element.
%% Examples:
%% ?- runs([a,a,b,b,b,c,c,d,d,d,d,e,e]).
%% true.
%% ?- runs([a,a,b,b,b,c,X,d,d,d,d,e,e]).
%% X=c
%% ?- runs([a,a,b,b,b,c,c,X,d,d,d,e,e]).
%% [25 marks]
%% CS424 Page 2 of 2 January 2019
%% X=c ;
%% X=d
%% ?- runs([a]).
%% false.
%% ?- runs([]).
%% true

runs([]).
runs([H|[T1|T2]]) :-
    H = T1,
    runs(T2).



%% Q3: Prolog
%%  Define a Prolog predicate scissor/4 which is true when a list
%%  (first arg) is split into two pieces (3rd and 4th arg) at a given
%%  element (third arg).
%%  Example:
%%  ?- scissors([a,b,c,d,e,f],c,[a,b],[d,e,f]).
%%  yes

scissors([H1|T1], C, [], P2) :-
    (H1 = C),
    secondP(T1, P2).
scissors([H1|T1], C, [H2|T2], P2) :-
    H1 = H2,
    scissors(T1, C, T2, P2).

secondP([], []).
secondP([H1|T1], [H2|T2]) :-
    H1 = H2,
    secondP(T1, T2).


triangleify(List, Out) :-
    length(List, Len),
    triangle(List, 0, Len, [], Out).
triangle([], L, L, ACC, OUT) :- ACC = OUT.
triangle([C|N], I, L, ACC, OUT) :-
    INT is L - I,
    INC is I + 1,
    take(C, INT, SUB),
    append(ACC, [SUB], NEW),
    triangle(N, INC, L, NEW, OUT).

%% Q4: Prolog
%% Define a Prolog predicate before/3(+,+,-) where before(XS,YS,ZS) is
%% true when the list ZS contains (in order) elements of YS which
%% immediately precede elements which are members of XS.
%% | ?- before([i],[k,j,h,i,u,i,i],[h,u,i]).

allElesBefore(_, [H|[]],  OUT, RES) :- OUT = RES.
allElesBefore(X, [H|[H2|T2]], OUT, RES) :-
    X = H2 ->
	append(OUT, [H], NEW),
	allElesBefore(X, [H2|T2], NEW, RES) ;
    allElesBefore(X, [H2|T2], OUT, RES).

before(XS, YS, ZS) :- beforeRec(XS, YS, [], ZS).

beforeRec([], _, ACC, ZS) :- ACC = ZS.
beforeRec([H|T], YS, ACC, ZS) :-
    allElesBefore(H, YS, [], OUT),
    append(ACC, OUT, NEW),
    beforeRec(T, YS, NEW, ZS).

    
	
    
