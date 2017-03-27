
% syntax:
% die(Color, Number).

color(C) :- member(C, [r, g, b, k]).
color(die(C,_), C) :- color(C).
num(N) :- member(N, [1, 2, 3, 4, 5, 6]).
num(die(_,N),N) :- num(N).

% queries:
% color(die(r,2), Color).
% num(die(r,2), Number).
% color(C).
% num(N).
% color(Die, Color).

