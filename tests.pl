% run as: swipl --traditional -s tests.pl -t run_tests

:- use_module(library(test_cover)).

:- begin_tests(defuser).
:- [defuser].
:- [gamecards].

test(cardStringToConstraints, [nondet]) :-
    cardString(3, [equal(die(c, n), die(c, n)), die(g, 4)],
        "{cn=cn,g4}", []),
    cardString(3, [equal(die(c, n), die(c, n), die(c, n))],
        "{cn=cn=cn}", []).

test(constraintsToPredicate, [nondet]) :-
    constraintsToPredicate([equal(die(c, n), die(c, n)), die(g, 4)],
        [[2, defused, 'cn=cn'], [1, defused, 'c#', g, 4]]).

test(cardDefused, [nondet]) :-
    cardDefused("{g6}", [die(g,6)]),
    cardDefused("{w#=w#}", [die(g,6), die(r,6)]),
    cardDefused("{w#=w#=w#}", [die(g,6), die(r,6), die(b,6)]),
    cardDefused("{cn=cn,w#=w#}", [die(g,6), die(g,3), die(r,3), die(b,3)]),
    cardDefused("{w6,w5,w1}", [die(g,6), die(r,5), die(b,1)]),
    cardDefused("{rn,w2,kn,w6}", [die(r,6), die(r,2), die(k,1), die(g,6)]).

test(numSolutions, [nondet]) :-
    numSolutions("{g6}", 1),
    numSolutions("{gn}", 6),
    numSolutions("{w6}", 4),
    numSolutions("{g6,r4}", 1),
    numSolutions("{g6,c#=c#}", 24),
    numSolutions("{w#=w#,cn=cn}", 13824),
    numSolutions("{wn+wn=4}", 48),
    numSolutions("{wn-wn=4}", 32).

% in these tests, we don't care what the generated string is, just that one was
% found (in finite time)
test(numSolutionsGenCardString, [nondet]) :-
    numSolutions(_, 1),
    numSolutions(_, 24),
    numSolutions(_, 32),
    numSolutions(_, 48),
    numSolutions(_, 13824).

:- end_tests(defuser).

