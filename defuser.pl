% constraint logic programming, for cards that use arithmetic
:- use_module(library(clpfd)).

% run as: swipl --traditional

% dice notation: die(Color,Number)
color(C) :- member(C, [r, g, b, k]).
color(die(C,_),C) :- color(C).
num(N) :- member(N, [1, 2, 3, 4, 5, 6]), N in 1..6. % uses clpfd
num(die(_,N),N) :- num(N).

% defused predicates don't appear in order according to arity
:- discontiguous defused/3.
:- discontiguous defused/4.

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
    { number_codes(N, [D,D2]) }.
arithmeticTerminal(N) --> digit(D),
    { number_codes(N, [D]), N in 0..9 }.
digitnonzero(D) --> [D], { D \= 48, code_type(D, digit) }.
digit(D) --> [D], { code_type(D, digit) }.

constraintsToPredicate([], []).
constraintsToPredicate([[Constraints]|Tail], [PredHead|PredTail]) :-
    constraintsToPredicate(Constraints, PredHead),
    constraintsToPredicate(Tail, PredTail).
constraintsToPredicate([Card|Tail], [PredHead|PredTail]) :-
    constraintsToPredicate(Card, PredHead),
    constraintsToPredicate(Tail, PredTail).
% returned val is [arg-count, pred-name, initial args...]
constraintsToPredicate(die(w, n), [1, defused, 'wn']).
constraintsToPredicate(die(w, Number), [1, defused, 'w#', Number]) :-
    Number \= n.
constraintsToPredicate(die(Color, n), [1, defused, 'cn', Color]) :-
    Color \= w.
constraintsToPredicate(die(Color, Number), [1, defused, 'c#', Color, Number]) :-
    Number \= n,
    Color \= w.
constraintsToPredicate(equal(die(c,n), die(c,n)), [2, defused, 'cn=cn']).
constraintsToPredicate(equal(die(w,#), die(w,#)), [2, defused, 'w#=w#']).
constraintsToPredicate(equal(die(c,#), die(c,#)), [2, defused, 'c#=c#']).
constraintsToPredicate(equal(die(c,n), die(c,n), die(c,n)), [3, defused, 'cn=cn=cn']).
constraintsToPredicate(equal(die(w,#), die(w,#), die(w,#)), [3, defused, 'w#=w#=w#']).
constraintsToPredicate(equal(die(c,#), die(c,#), die(c,#)), [3, defused, 'c#=c#=c#']).
constraintsToPredicate(add(die(w,n), die(w,n), N), [2, defused, 'wn+wn=N', N]).
constraintsToPredicate(subtract(die(w,n), die(w,n), N), [2, defused, 'wn-wn=N', N]).

% split_at from: https://github.com/mndrix/list_util/blob/master/prolog/list_util.pl
split_at(N,Xs,Take,Rest) :-
    split_at_(Xs,N,Take,Rest).
split_at_(Rest, 0, [], Rest) :- !. % optimization
split_at_([], N, [], []) :-
    N > 0.
split_at_([X|Xs], N, [X|Take], Rest) :-
    N > 0,
    succ(N0, N),
    split_at_(Xs, N0, Take, Rest).

checkAllPredicates([], []). % require no args (dice) left if run out of predicates
checkAllPredicates([[ArgCount,PredName|InitialArgs]|Tail], Args) :-
    split_at(ArgCount, Args, ExtraArgs, RestArgs),
    append(InitialArgs, ExtraArgs, GoalArgs),
    Goal =.. [PredName|GoalArgs],
    call(Goal),
    checkAllPredicates(Tail, RestArgs).

cardDefused(CardString, Dice) :-
    DiceCount in 1..5,
    length(Dice, DiceCount),
    cardString(DiceCount, Constraints, CardString, []),
    constraintsToPredicate(Constraints, Predicates),
    checkAllPredicates(Predicates, Dice).

findFDVars([], []).
findFDVars([die(_, Number)|Dice], [Number|FDVars]) :-
    fd_var(Number), !, % don't backtrack off this
    findFDVars(Dice, FDVars).
findFDVars([_|Dice], FDVars) :-
    findFDVars(Dice, FDVars).

numSolutions(CardString, NumSolutions) :-
    % force CardString to be instantiated first
    cardString(DiceCount, Constraints, CardString, []),
    constraintsToPredicate(Constraints, Predicates),
    % generate the right number of Dice
    length(Dice, DiceCount),
    % then count solutions for the CardString and Dice
    aggregate_all(count, (checkAllPredicates(Predicates, Dice),
                          findFDVars(Dice, FDVars),
                          label(FDVars)),
                  NumSolutions).

% run on command line:
% swipl --traditional -q -s defuser.pl -t enumerateCardStringsAndNumSolutions
% redirect output to CSV file
enumerateCardStringsAndNumSolutions :-
    numSolutions(CardString, NumSolutions),
    % retrieve the dice count
    cardString(DiceCount, _, CardString, []),
    format("\"~s\", ~p, ~p~n", [CardString, DiceCount, NumSolutions]),
    fail.

