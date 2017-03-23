
% run as: swipl --traditional

% dice notation: die(Color,Number)
color(die(C,_),C) :- member(C, [r, g, b, k]).
num(die(_,N),N) :- member(N, [1, 2, 3, 4, 5, 6]).

:- discontiguous defused/4. % defused predicates don't appear in order according to arity

defused('wn', _, _, _).
defused('cn', Color, _, A) :-
    color(A, Color).
defused('w#', _, Number, A) :-
    num(A, Number).
defused('c#', Color, Number, A) :-
    color(A, Color), num(A, Number).

defused('cn=cn', A, B) :-
    color(A, Color), color(B, Color).
defused('w#=w#', A, B) :-
    num(A, Num), num(B, Num).
defused('c#=c#', A, B) :-
    color(A, Color), color(B, Color),
    num(A, Num), num(B, Num).

defused('cn=cn=cn', A, B, C) :-
    color(A, Color), color(B, Color), color(C, Color).
defused('w#=w#=w#', A, B, C) :-
    num(A, Num), num(B, Num), num(C, Num).
defused('c#=c#=c#', A, B, C) :-
    color(A, Color), color(B, Color), color(C, Color),
    num(A, Num), num(B, Num), num(C, Num).

cardString(Constraints) --> "{", exprSeq(Constraints), "}".

% need to explicitly expand all the recursion to be sure we don't generate
% infinite card strings
exprSeq(Constraints) --> expr(Constraints).
exprSeq(Constraints) --> expr(LeftConstraint), ",", exprSeq2(RightConstraint),
    { append(LeftConstraint, RightConstraint, Constraints) }.
exprSeq2(Constraints) --> expr(Constraints).
exprSeq2(Constraints) --> expr(LeftConstraint), ",", exprSeq3(RightConstraint),
    { append(LeftConstraint, RightConstraint, Constraints) }.
exprSeq3(Constraints) --> expr(Constraints).
exprSeq3(Constraints) --> expr(LeftConstraint), ",", exprSeq4(RightConstraint),
    { append(LeftConstraint, RightConstraint, Constraints) }.
exprSeq4(Constraints) --> expr(Constraints).
exprSeq4(Constraints) --> expr(LeftConstraint), ",", exprSeq5(RightConstraint),
    { append(LeftConstraint, RightConstraint, Constraints) }.
exprSeq5(Constraints) --> expr(Constraints).

compatibleConstraint(equal(die(c, n), die(c, n))).
compatibleConstraint(equal(die(w, #), die(w, #))).
compatibleConstraint(equal(die(c, #), die(c, #))).
compatibleConstraint(equal(die(c, n), die(c, n), die(c, n))).
compatibleConstraint(equal(die(w, #), die(w, #), die(w, #))).
compatibleConstraint(equal(die(c, #), die(c, #), die(c, #))).

expr([Constraints]) --> exprTerminal(Constraints).
expr([equal(LeftConstraint, RightConstraint)]) -->
    exprTerminal(LeftConstraint), "=", exprTerminal(RightConstraint),
    { compatibleConstraint(equal(LeftConstraint, RightConstraint)) }.
expr([equal(LeftConstraint, MiddleConstraint, RightConstraint)]) -->
    exprTerminal(LeftConstraint), "=", exprTerminal(MiddleConstraint),
    "=", exprTerminal(RightConstraint),
    { compatibleConstraint(equal(LeftConstraint, MiddleConstraint, RightConstraint)) }.
%expr([notequal(LeftConstraint, RightConstraint)]) -->
%    exprTerminal(LeftConstraint), "!=", exprTerminal(RightConstraint).
%expr([above(LeftConstraint, RightConstraint)]) -->
%    exprTerminal(LeftConstraint), "^", exprTerminal(RightConstraint).

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

constraintsToPredicate([], []).
constraintsToPredicate([[Constraints]|Tail], [PredHead|PredTail]) :-
    constraintsToPredicate(Constraints, PredHead),
    constraintsToPredicate(Tail, PredTail).
constraintsToPredicate([Card|Tail], [PredHead|PredTail]) :-
    constraintsToPredicate(Card, PredHead),
    constraintsToPredicate(Tail, PredTail).
% returned val is [arg-count, pred-name, initial args...]
constraintsToPredicate(die(w, n), [1, defused, 'wn', w, n]).
constraintsToPredicate(die(w, Number), [1, defused, 'w#', w, Number]) :-
    Number \= n.
constraintsToPredicate(die(Color, n), [1, defused, 'cn', Color, n]) :-
    Color \= c.
constraintsToPredicate(die(Color, Number), [1, defused, 'c#', Color, Number]) :-
    Number \= n,
    Color \= c.
constraintsToPredicate(equal(die(c,n), die(c,n)), [2, defused, 'cn=cn']).
constraintsToPredicate(equal(die(w,#), die(w,#)), [2, defused, 'w#=w#']).
constraintsToPredicate(equal(die(c,#), die(c,#)), [2, defused, 'c#=c#']).
constraintsToPredicate(equal(die(c,n), die(c,n), die(c,n)), [3, defused, 'cn=cn=cn']).
constraintsToPredicate(equal(die(w,#), die(w,#), die(w,#)), [3, defused, 'w#=w#=w#']).
constraintsToPredicate(equal(die(c,#), die(c,#), die(c,#)), [3, defused, 'c#=c#=c#']).

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
    length(ExtraArgs, ArgCount), % ensure we got enough arguments
    append(InitialArgs, ExtraArgs, GoalArgs),
    Goal =.. [PredName|GoalArgs],
    call(Goal),
    checkAllPredicates(Tail, RestArgs).

cardDefused(CardString, Dice) :-
    cardString(Constraints, CardString, []),
    constraintsToPredicate(Constraints, Predicates),
    checkAllPredicates(Predicates, Dice).

countDieSolutions(die(Color, Number), 1) :-
    ground(Color), ground(Number).
countDieSolutions(die(Color, Number), 6) :-
    ground(Color), var(Number).
countDieSolutions(die(Color, Number), 4) :-
    var(Color), ground(Number).

countDiceSolutions([Die], NumSolutions) :-
    countDieSolutions(Die, NumSolutions).
countDiceSolutions([Die|Dice], NumSolutions) :-
    countDieSolutions(Die, DieSolutions),
    countDiceSolutions(Dice, RestSolutions),
    NumSolutions is DieSolutions * RestSolutions.

countMultipleDiceSolutions([], 0).
countMultipleDiceSolutions([Dice|Rest], NumSolutions) :-
    countDiceSolutions(Dice, HeadSolutions),
    countMultipleDiceSolutions(Rest, RestSolutions),
    NumSolutions is HeadSolutions + RestSolutions.

numSolutions(CardString, NumSolutions) :-
    setof(Dice, cardDefused(CardString, Dice), Solutions),
    countMultipleDiceSolutions(Solutions, NumSolutions), !.

