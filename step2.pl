
color(C) :- member(C, [r, g, b, k]).
color(die(C,_), C) :- color(C).
num(N) :- member(N, [1, 2, 3, 4, 5, 6]).
num(die(_,N),N) :- num(N).

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

% queries:
% cardString("{g6}", []).
% cardString("{g6,wn=wn}", []).
% cardString("{g6,wn=wn,b8}", []).
% cardString(CS, []), format("~s~n", [CS]).



