
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

cardString(DiceCount, Constraints) --> "{", exprSeq(DiceCount, Constraints), "}".

exprSeq(DiceCount, Constraints) --> expr(DiceCount, Constraints).
exprSeq(DiceCount, Constraints) -->
    { DiceCountLeft in 1..5,
      DiceCountRight in 1..5,
      DiceCount in 1..5,
      DiceCount #= DiceCountLeft + DiceCountRight },
    expr(DiceCountLeft, LeftConstraint), ",", exprSeq(DiceCountRight, RightConstraint),
    { append(LeftConstraint, RightConstraint, Constraints) }.

% ensure proper color/number (only generic variables w and n, not c or #)
compatibleSingleDie(die(w,n)).
compatibleSingleDie(D) :- color(D, _), num(D, _).
compatibleSingleDie(die(w,N)) :- num(N).
compatibleSingleDie(die(C,n)) :- color(C).

compatibleConstraint(equal(die(c, n), die(c, n))).
compatibleConstraint(equal(die(w, #), die(w, #))).
compatibleConstraint(equal(die(c, #), die(c, #))).
compatibleConstraint(equal(die(c, n), die(c, n), die(c, n))).
compatibleConstraint(equal(die(w, #), die(w, #), die(w, #))).
compatibleConstraint(equal(die(c, #), die(c, #), die(c, #))).

% currently, only wn+/-wn is support for arithmetic; ultimately should support more
compatibleArithmeticConstraint(die(w, n), die(w, n)).

expr(2, [equal(LeftDie, RightDie)]) -->
    { compatibleConstraint(equal(LeftDie, RightDie)) },
    exprTerminal(LeftDie), "=", exprTerminal(RightDie).
expr(3, [equal(LeftDie, MiddleDie, RightDie)]) -->
    { compatibleConstraint(equal(LeftDie, MiddleDie, RightDie)) },
    exprTerminal(LeftDie), "=", exprTerminal(MiddleDie), "=", exprTerminal(RightDie).
expr(2, [add(LeftDie, RightDie, N)]) -->
    { compatibleArithmeticConstraint(LeftDie, RightDie), N in 2..12 },
    exprTerminal(LeftDie), "+", exprTerminal(RightDie), "=", arithmeticTerminal(N).
expr(2, [subtract(LeftDie, RightDie, N)]) -->
    { compatibleArithmeticConstraint(LeftDie, RightDie), N in 0..5 },
    exprTerminal(LeftDie), "-", exprTerminal(RightDie), "=", arithmeticTerminal(N).
% put this last since it has the most variability (every color/number)
expr(1, [Die]) -->
    { compatibleSingleDie(Die) },
    exprTerminal(Die).

exprTerminal(die(Color, Number)) --> colorTerminal(Color), numberTerminal(Number).

colorTerminal(k) --> "k". % black
colorTerminal(r) --> "r". % red
colorTerminal(g) --> "g". % green
colorTerminal(b) --> "b". % blue
colorTerminal(w) --> "w". % don't care color
colorTerminal(c) --> "c". % matching color

numberTerminal(1) --> "1".
numberTerminal(2) --> "2".
numberTerminal(3) --> "3".
numberTerminal(4) --> "4".
numberTerminal(5) --> "5".
numberTerminal(6) --> "6".
numberTerminal(n) --> "n". % don't care number
numberTerminal(#) --> "#". % matching number

% only allow 1 or 2 digit numbers
arithmeticTerminal(N) --> digitnonzero(D), digit(D2),
    { number_codes(N, [D,D2]), N in 0..18 }.
arithmeticTerminal(N) --> digit(D),
    { number_codes(N, [D]), N in 0..9 }.
digitnonzero(D) --> [D],
    { code_type(D, digit), D \= 48 }.
digit(D) --> [D], { code_type(D, digit) }.

% queries:
% expr(DC, C, "w#=w#", []).
% exprSeq(DC, C, "g6,b2", []).
% exprSeq(3, [die(b,2), equal(die(c,#),die(c,#))], CS, []), format...
% exprSeq(3, C, CS, []), format...
% exprSeq(D, C, CS, []), format...
% cardString(D, C, CS, []), format...

