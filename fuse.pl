
% cards use notation: c(Color,Number)

color(c(C,_),C).
num(c(_,N),N).

% color exact match
colorMatch(c(Color,_), Color).
% color variable match
colorMatch(_, "w").

% number exact match
numMatch(c(_, Number), Number).
% number variable match
numMatch(_, "#").
    
% exact color and number match for single die
defused('cn', Color, Number, A) :- color(A, Color), num(A, Number).

% 'equal' case 1: color constrained: c#=c#
defused('c#=c#', A, B) :- color(A, Color), color(B, Color).
% 'equal' case 2: number constrained: wn=wn
defused('wn=wn', A, B) :- num(A, Num), num(B, Num).
% 'equal' case 3: color and number constrained: cn=cn
defused('cn=cn', A, B) :- color(A, Color), color(B, Color), num(A, Num), num(B, Num).

cardString(Constraints) --> "{", exprSeq(Constraints), "}".

exprSeq(Constraints) --> expr(Constraints).
% comma=and, so constraints come out as a list (Prolog 'and')
exprSeq([LeftConstraint, RightConstraint]) --> expr(LeftConstraint), ",", exprSeq(RightConstraint).

expr(Constraints) --> exprTerminal(Constraints).
expr(equal(LeftConstraint, RightConstraint)) --> exprTerminal(LeftConstraint), "=", exprTerminal(RightConstraint).
expr([equal(LeftConstraint, MiddleConstraint), equal(MiddleConstraint, RightConstraint)]) --> exprTerminal(LeftConstraint), "=", exprTerminal(MiddleConstraint), "=", exprTerminal(RightConstraint).
expr(notequal(LeftConstraint, RightConstraint)) --> exprTerminal(LeftConstraint), "!=", exprTerminal(RightConstraint).
expr(above(LeftConstraint, RightConstraint)) --> exprTerminal(LeftConstraint), "^", exprTerminal(RightConstraint).

exprTerminal(c(Color, Number)) --> colorTerminal(Color), numberTerminal(Number).

colorTerminal("w") --> "w". % don't care color
colorTerminal("c") --> "c". % specific but unspecified color
colorTerminal("b") --> "b". % black
colorTerminal("r") --> "r". % red
colorTerminal("g") --> "g". % green
colorTerminal("u") --> "u". % blue

numberTerminal("#") --> "#". % don't care number
numberTerminal("n") --> "n". % specific but unspecified number
numberTerminal("1") --> "1".
numberTerminal("2") --> "2".
numberTerminal("3") --> "3".
numberTerminal("4") --> "4".
numberTerminal("5") --> "5".
numberTerminal("6") --> "6".

card(c(Color,Number)) :-
    member(Color, ["w","c","b","r","g","u"]),
    member(Number, ["#","n","1","2","3","4","5","6"]).

% e.g.:
% ?- string_codes("{c#=c#=c#,g4}", X), cardString(Constraints, X, []), constraintsToPredicate(Constraints, Predicate).
% Constraints = [[equal(c("c", "#"), c("c", "#")), equal(c("c", "#"), c("c", "#"))], c("g", "4")],
% Predicate = [color(card47, _G2317), color(card48, _G2317), color(card49, _G2335), color(card50, _G2335), colorMatch(card51, "g"), numMatch(card51, "4")] .

constraintsToPredicate([], []).
constraintsToPredicate([[Constraints]|Tail], [PredHead|PredTail]) :-
    constraintsToPredicate(Constraints, PredHead),
    constraintsToPredicate(Tail, PredTail).
constraintsToPredicate([Card|Tail], [PredHead|PredTail]) :-
    constraintsToPredicate(Card, PredHead),
    constraintsToPredicate(Tail, PredTail).
% returned val is [arg-count, pred-name, initial args...]
% literal card, must match color and number (either/both of which could be variable)
constraintsToPredicate(c(Color, Number), [1, defused, 'cn', Color, Number]).
% translate equal(...) constraints to defused predicates
constraintsToPredicate(equal(c("c","#"), c("c","#")), [2, defused, 'c#=c#']).
constraintsToPredicate(equal(c("w","n"), c("w","n")), [2, defused, 'wn=wn']).
constraintsToPredicate(equal(c("c","n"), c("c","n")), [2, defused, 'cn=cn']).

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

checkAllPredicates([], _).
checkAllPredicates([[ArgCount,PredName|InitialArgs]|Tail], Args) :-
    split_at(ArgCount, Args, ExtraArgs, RestArgs),
    append(InitialArgs, ExtraArgs, GoalArgs),
    Goal =.. [PredName|GoalArgs],
    call(Goal),
    checkAllPredicates(Tail, RestArgs).

% example usage: cardDefused("{g6,cn=cn}", [G,A,c("r",N)]).
% result: G = c("g", "6"), A = c("r", N)
cardDefused(CardString, Dice) :-
    string_codes(CardString, AsciiCodes),
    cardString(Constraints, AsciiCodes, []),
    print(Constraints), nl,
    constraintsToPredicate(Constraints, Predicates),
    print(Predicates), nl,
    checkAllPredicates(Predicates, Dice).


