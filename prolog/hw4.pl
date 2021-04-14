%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   HW4                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Q1                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Q2                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
