% syntax: https://gist.github.com/Maldaris/eb153bb31104bd76a31706ec42e5d01e

% dice use notation: die(Color,Number)

color(die(C,_),C) :- member(C, [r, g, b, u]).
num(die(_,N),N) :- member(N, [1, 2, 3, 4, 5, 6]).

% color exact match
colorMatch(die(Color,_), Color).
% color variable match
colorMatch(_, w).

% number exact match
numMatch(die(_, Number), Number).
% number variable match
numMatch(_, #).
    
% exact color and number match for single die
defused('cn', Color, Number, A) :- color(A, Color), num(A, Number).

% 'equal' case 1: color constrained: c#=c#
defused('c#=c#', A, B) :- color(A, Color), color(B, Color).
% 'equal' case 2: number constrained: wn=wn
defused('wn=wn', A, B) :- num(A, Num), num(B, Num).
% 'equal' case 3: color and number constrained: cn=cn
defused('cn=cn', A, B) :- color(A, Color), color(B, Color), num(A, Num), num(B, Num).

cardString(Constraints) --> "{", exprSeq(Constraints), "}", { length(Constraints, L), L =< 5 }.

exprSeq(Constraints) --> expr(Constraints).
% comma=and, so constraints come out as a list (Prolog 'and')
exprSeq(Constraints) --> expr(LeftConstraint), ",", exprSeq(RightConstraint), { append(LeftConstraint, RightConstraint, Constraints) }.

expr([Constraints]) --> exprTerminal(Constraints).
expr([equal(LeftConstraint, RightConstraint)]) --> exprTerminal(LeftConstraint), "=", exprTerminal(RightConstraint).
%expr([equal(LeftConstraint, MiddleConstraint), equal(MiddleConstraint, RightConstraint)]) --> exprTerminal(LeftConstraint), "=", exprTerminal(MiddleConstraint), "=", exprTerminal(RightConstraint).
%expr([notequal(LeftConstraint, RightConstraint)]) --> exprTerminal(LeftConstraint), "!=", exprTerminal(RightConstraint).
%expr([above(LeftConstraint, RightConstraint)]) --> exprTerminal(LeftConstraint), "^", exprTerminal(RightConstraint).

exprTerminal(die(Color, Number)) --> colorTerminal(Color), numberTerminal(Number).

colorTerminal(b) --> "b". % black
colorTerminal(r) --> "r". % red
colorTerminal(g) --> "g". % green
colorTerminal(u) --> "u". % blue
colorTerminal(w) --> "w". % don't care color
colorTerminal(c) --> "c". % specific but unspecified color

numberTerminal(1) --> "1".
numberTerminal(2) --> "2".
numberTerminal(3) --> "3".
numberTerminal(4) --> "4".
numberTerminal(5) --> "5".
numberTerminal(6) --> "6".
numberTerminal(#) --> "#". % don't care number
numberTerminal(n) --> "n". % specific but unspecified number

% e.g.:
% ?- cardString(Constraints, "{c#=c#=c#,g4}", []), constraintsToPredicate(Constraints, Predicate).
% Constraints = [[equal(die(c, #), die(c, #)), equal(die(c, #), die(c, #))], die(g, 4)],
% Predicate = [[[2, defused, 'c#=c#'], [2, defused, 'c#=c#']], [1, defused, cn, g, 4]] ;

constraintsToPredicate([], []).
constraintsToPredicate([[Constraints]|Tail], [PredHead|PredTail]) :-
    constraintsToPredicate(Constraints, PredHead),
    constraintsToPredicate(Tail, PredTail).
constraintsToPredicate([Card|Tail], [PredHead|PredTail]) :-
    constraintsToPredicate(Card, PredHead),
    constraintsToPredicate(Tail, PredTail).
% returned val is [arg-count, pred-name, initial args...]
% literal card, must match color and number (either/both of which could be variable)
constraintsToPredicate(die(Color, Number), [1, defused, 'cn', Color, Number]).
% translate equal(...) constraints to defused predicates
constraintsToPredicate(equal(die(c,#), die(c,#)), [2, defused, 'c#=c#']).
constraintsToPredicate(equal(die(w,n), die(w,n)), [2, defused, 'wn=wn']).
constraintsToPredicate(equal(die(c,n), die(c,n)), [2, defused, 'cn=cn']).

% split_at from: https://github.com/mndrix/list_util/blob/master/prolog/list_util.pl
split_at(N,Xs,Take,Rest) :-
    split_at_(Xs,N,Take,Rest).
split_at_(Rest, 0, [], Rest) :- !. % optimization
split_at_([], N, [], []) :-
    % cannot optimize here because (+, -, -, -) would be wrong,
    % which could possibly be a useful generator.
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
    print(Goal), nl,
    call(Goal),
    checkAllPredicates(Tail, RestArgs).

% example usage: cardDefused("{g6,cn=cn}", [G,A,die(r,N)]).
% result:
% G = die(g, 6),
% A = die(r, 1),
% N = 1 ;
% etc.
cardDefused(CardString, Dice) :-
    cardString(Constraints, CardString, []),
    %length(Constraints, CLength),
    %length(Dice, DLength),
    %CLength =< DLength, % must have equal or fewer constraints than dice (each constraint takes 1+ dice)
    constraintsToPredicate(Constraints, Predicates),
    print('predicates: '), print(Predicates), nl,
    checkAllPredicates(Predicates, Dice).

% findall([G,A,B], cardDefused("{g6,cn=cn}", [G,A,B]), Solutions), length(Solutions, NumSolutions).
% Solutions = [[die(g, 6), die(r, 1), die(r, 1)], [die(g, 6), die(r, 2), die(r, 2)], [die(g, 6), die(r, 3), die(r, 3)], [die(g, 6), die(r, 4), die(r, 4)], [die(g, 6), die(r, 5), die(r, 5)], [die(g, 6), die(r, 6), die(..., ...)], [die(g, 6), die(..., ...)|...], [die(..., ...)|...], [...|...]|...],
% NumSolutions = 24.

% cardDefused(CardString, [die(g,6), die(r,6)]), format("~s~n", [CardString]).
% prints: {wn=wn}


% something like this won't work:
% findall([CardString, N], cardDefused(CardString, [die(g,N), die(r,N)]), Solutions).
% because even though it will (quickly) find six dice that work with string {wn=wn}, hence six solutions, it won't know that's all there is...
% unless we can better constrain the parser/predicate-generator to not generate longer strings than required
% (only have two dice, so at most two predicates?); need to do this at the parser level, if possible, so arbitrarily-long strings are not generated
% -- perhaps limit size of strings? cards are only so big...
% ** issue appears when backtracking over exprSeq which has recursive exprSeq on right side; if on left side, would have infinite-recursion on normal parsing, but since it's on the right side, we have infinite recursion on backtracking (unparsing)
% instead of supporting arbitrary recursion, could specify, say, five different exprSeq rules so it bottoms out

