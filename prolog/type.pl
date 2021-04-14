%% -*- prolog -*-

%% type checker for the simply-typed λ Calculus

%% term:
%%   e ::= v | b | e e | (λ v:t . e)
%% basis object
%%   b ::= true | false | 1 | 2 | plus | times | equal
%% types:
%%   t ::= (t -> t) | Bool | Int
%% basis types
%%   gamma0 ::= true : Bool , false : Bool, 1 : Int, plus : Int -> Int -> Int, equal : Int -> Int -> Bool

%% Representation for type environment...
%%   [[true,bool],[false,bool],[1,int],[plus,int->int]]

%% represent
%%    e1 e2 = [E1,E2]
%%    e1 e2 e3 = [[E1,E2],E3]
%%    λ v:t . e = lam(V,T,E)
gam([[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]]).
%% typeis(B,T,Gam) :- member([B,T],Gam).
%% typeis(V,T,Gam) :- member([V,T],Gam).

%% typeis(BorV,T,Gam) :- member([BorV,T],Gam). % needs :- isVar(BorV) or isBasis(BorV), ...
typeis(BorV,T,Gam) :- member([BorV,T1],Gam), !, T=T1.

typeis([E1,E2], T2, Gam) :- typeis(E1, (T1->T2), Gam), typeis(E2, T1, Gam).
typeis(lam(V,T1,E), (T1->T2), Gam) :- typeis(E, T2, [[V,T1]|Gam]). % needs :- isVar(V), ...

typeTop(E, T) :- gam(G), typeis(E, T, G).

%% X+Y is sugar for '+'(X,Y).
%% [a,b,c] is sugar for '.'(a,'.'(b,'.'(c,[])))

%%% Bug: finds "inner" then "outer" type of x, should find only "inner"!

%% | ?- typeTop(lam(x,int,lam(x,bool,x)), T).

%% T = (int->bool->bool) ? ;

%% T = (int->bool->int) ? ;

%% no


%%% Typescript

%% GNU Prolog 1.4.5 (64 bits)

%% | ?- X='.'(a,'.'(b,[])).

%% X = [a,b]

%% (1 ms) yes
%% | ?- X='+'(2,3).

%% X = 2+3

%% yes
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 28 lines read - 2506 bytes written, 11 ms

%% (1 ms) yes
%% | ?- gam(G).

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int)]]

%% yes
%% | ?- gam(G), typeis(true, bool, G).

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int)]] ? ;

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int)]] ? ;

%% no
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 29 lines read - 2268 bytes written, 9 ms

%% yes
%% | ?- gam(G), typeis(true, bool, G).

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int)]] ? ;

%% no
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 31 lines read - 2615 bytes written, 5 ms

%% (1 ms) yes
%% | ?- typeTop(true, bool).

%% true ? ;

%% no
%% | ?- typeTop(1, bool).

%% no
%% | ?- typeTop(1, int).

%% true ? ;

%% no
%% | ?- typeTop(1, T).

%% T = int ? ;

%% no
%% | ?- typeTop(plus, T).

%% T = (int->int)

%% yes
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 31 lines read - 2673 bytes written, 12 ms

%% yes
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 31 lines read - 2673 bytes written, 9 ms

%% (1 ms) yes
%% | ?- typeTop(plus, T).

%% T = (int->int->int)

%% yes
%% | ?- typeTop([plus,1], T).

%% T = (int->int) ? ;

%% no
%% | ?- typeTop([[plus,1],1], T).

%% T = int ? ;

%% no
%% | ?- typeTop(lam(x,int,[[plus,x],1]), T).

%% T = (int->int) ? ;

%% no
%% | ?- typeTop(lam(x,int,[[equal,x],1]), T).

%% no
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 31 lines read - 2979 bytes written, 9 ms

%% yes
%% | ?- typeTop(lam(x,int,[[equal,x],1]), T).

%% T = (int->bool) ? ;

%% (1 ms) no
%% | ?- typeTop(lam(x,TX,[[equal,x],1]), T).

%% T = (int->bool)
%% TX = int ? 

%% yes
%% | ?- typeTop(E, bool).

%% E = true ? ;

%% E = false ? ;

%% E = [[equal,1],1] ? ;

%% E = [[equal,1],[[plus,1],1]] ? ;

%% E = [[equal,1],[[plus,1],[[plus,1],1]]] ? ;

%% E = [[equal,1],[[plus,1],[[plus,1],[[plus,1],1]]]] ? ;

%% E = [[equal,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],1]]]]] ? ;

%% E = [[equal,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],1]]]]]] ? ;

%% E = [[equal,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],1]]]]]]] ? ;

%% E = [[equal,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],1]]]]]]]] ? ;

%% E = [[equal,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],[[plus,1],1]]]]]]]]] ? 

%% (2 ms) yes
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 31 lines read - 2979 bytes written, 119 ms

%% (4 ms) yes
%% | ?- typeTop(E, int).

%% E = 1 ? ;

%% E = [lam(A,int,A),1] ? ;

%% E = [lam(A,int,A),[lam(B,int,B),1]] ? ;

%% E = [lam(A,int,A),[lam(B,int,B),[lam(C,int,C),1]]] ? ;

%% E = [lam(A,int,A),[lam(B,int,B),[lam(C,int,C),[lam(D,int,D),1]]]] ? ;

%% E = [lam(A,int,A),[lam(B,int,B),[lam(C,int,C),[lam(D,int,D),[lam(F,int,F),1]]]]] ? ;

%% E = [lam(A,int,A),[lam(B,int,B),[lam(C,int,C),[lam(D,int,D),[lam(F,int,F),[lam(G,int,G),1]]]]]] ? 

%% yes
%% | ?- typeTop(E, bool).

%% E = true ? ;

%% E = false ? ;

%% E = [lam(A,bool,A),true] ? ;

%% E = [lam(A,bool,A),false] ? ;

%% E = [lam(A,bool,A),[lam(B,bool,B),true]] ? ;

%% E = [lam(A,bool,A),[lam(B,bool,B),false]] ? ;

%% E = [lam(A,bool,A),[lam(B,bool,B),[lam(C,bool,C),true]]] ? ;

%% E = [lam(A,bool,A),[lam(B,bool,B),[lam(C,bool,C),false]]] ? ;

%% E = [lam(A,bool,A),[lam(B,bool,B),[lam(C,bool,C),[lam(D,bool,D),true]]]] ? ;

%% E = [lam(A,bool,A),[lam(B,bool,B),[lam(C,bool,C),[lam(D,bool,D),false]]]] ? ;

%% E = [lam(A,bool,A),[lam(B,bool,B),[lam(C,bool,C),[lam(D,bool,D),[lam(F,bool,F),true]]]]] ? ;

%% E = [lam(A,bool,A),[lam(B,bool,B),[lam(C,bool,C),[lam(D,bool,D),[lam(F,bool,F),false]]]]] ? 

%% (4 ms) yes
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 31 lines read - 2979 bytes written, 7 ms

%% (2 ms) yes
%% | ?- typeTop(lam(x,bool,x), T).

%% T = (bool->bool) ? ;

%% no
%% | ?- typeTop(lam(x,int,x), T).

%% T = (int->int) ? ;

%% no
%% | ?- typeTop(lam(x,int,lam(y,bool,y)), T).

%% T = (int->bool->bool) ? ;

%% no
%% | ?- typeTop(lam(x,int,lam(y,bool,x)), T).

%% T = (int->bool->int) ? ;

%% no
%% | ?- typeTop(lam(x,int,lam(x,bool,x)), T).

%% T = (int->bool->bool) ? ;

%% T = (int->bool->int) ? ;

%% no
%% | ?- gam(G), member([false,T], G).

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]]
%% T = bool ? ;

%% no
%% | ?- gam(G), member([E,bool], G).

%% E = true
%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]] ? ;

%% E = false
%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]] ? ;

%% (1 ms) no
%% | ?- gam(G), member([x,T], [[x,int],G]).

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]]
%% T = int ? ;

%% (1 ms) no
%% | ?- gam(G), member([x,T], [[x,int],[x,bool],G]).

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]]
%% T = int ? ;

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]]
%% T = bool ? ;

%% no
%% | ?- gam(G), member([x,T], [[x,int],[x,bool],G]), !.

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]]
%% T = int

%% (1 ms) yes
%% | ?- gam(G), member([x,int], [[x,int],[x,bool],G]), !.

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]]

%% yes
%% | ?- gam(G), member([x,bool], [[x,int],[x,bool],G]), !.

%% G = [[true,bool],[false,bool],[1,int],[plus,(int->int->int)],[equal,(int->int->bool)]]

%% yes
%% | ?- ['lecture-22.pl'].
%% compiling /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl for byte code...
%% /home/barak/text/Teach/NUIM/2020/cs424/lecture-22.pl compiled, 44 lines read - 3151 bytes written, 60 ms

%% (2 ms) yes
%% | ?- typeTop(lam(x,int,lam(x,bool,x)), T).

%% T = (int->bool->bool)

%% yes
