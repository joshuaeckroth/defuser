
:- use_module(library(clpfd)).

color(C) :- member(C, [r, g, b, k]).
color(die(C,_), C) :- color(C).
num(N) :- member(N, [1, 2, 3, 4, 5, 6]), N in 1..6.
num(die(_,N),N) :- num(N).

:- discontiguous defused/3, defused/4.

defused('wn', die(C, N)) :- color(C), num(N).
defused('cn', Color, die(Color, N)) :- color(Color), num(N).
defused('w#', Number, die(C, Number)) :- color(C), num(Number).
defused('c#', Color, Number, die(Color, Number)) :- color(Color), num(Number).

defused('cn=cn', A, B) :- color(A, Color), color(B, Color), num(A, _), num(B, _).
defused('w#=w#', A, B) :- color(A, _), color(B, _), num(A, Num), num(B, Num).
defused('c#=c#', A, B) :- color(A, Color), color(B, Color), num(A, Num), num(B, Num).

defused('cn=cn=cn', A, B, C) :-
    color(A, Color), color(B, Color), color(C, Color),
    num(A, _), num(B, _), num(C, _).
defused('w#=w#=w#', A, B, C) :-
    color(A, _), color(B, _), color(C, _),
    num(A, Num), num(B, Num), num(C, Num).
defused('c#=c#=c#', A, B, C) :-
    color(A, Color), color(B, Color), color(C, Color),
    num(A, Num), num(B, Num), num(C, Num).

defused('wn+wn=N', N, A, B) :-
    color(A, _), color(B, _),
    num(A, NumA), num(B, NumB), N #= NumA + NumB.
defused('wn-wn=N', N, A, B) :-
    color(A, _), color(B, _),
    num(A, NumA), num(B, NumB), N #= NumA - NumB.
    
% queries:
% defused('wn', die(g,6)).
% defused('cn=cn', die(g,6), die(r,2)).
% defused('cn=cn', die(g,6), D).
% defused('w#=w#=w#', die(g,6), D, E).
% defused(C, die(g,6)).
% defused(C, die(g,6), D).
% defused('wn+wn=N', 8, die(g,6), die(r,2)).
% defused('wn+wn=N', 8, dig(g,6), D).
% defused('wn+wn=N', N, D, E).
% defused(C, 3, D, E).

cardString --> "{", exprSeq, "}".

exprSeq --> expr.
exprSeq --> expr, ",", exprSeq.

expr --> exprTerminal, "=", exprTerminal.
expr --> exprTerminal, "=", exprTerminal, "=", exprTerminal.
expr --> exprTerminal, "+", exprTerminal, "=", arithmeticTerminal.
expr --> exprTerminal, "-", exprTerminal, "=", arithmeticTerminal.
expr --> exprTerminal.

exprTerminal --> colorTerminal, numberTerminal.

colorTerminal --> "k".
colorTerminal --> "r".
colorTerminal --> "g".
colorTerminal --> "b".
colorTerminal --> "w". % don't care color
colorTerminal --> "c". % matching color

numberTerminal --> "1".
numberTerminal --> "2".
numberTerminal --> "3".
numberTerminal --> "4".
numberTerminal --> "5".
numberTerminal --> "6".
numberTerminal --> "n". % don't care number
numberTerminal --> "#". % matching number

arithmeticTerminal --> digitnonzero, digit.
arithmeticTerminal --> digit.
digitNonZero --> [D], { code_type(D, digit), D \= 48 }.
digit --> [D], { code_type(D, digit) }.

