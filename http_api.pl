% run as: swipl --traditional -s http_api.pl -g 'server(10333)'

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).

:- [defuser].

server(Port) :- http_server(http_dispatch, [port(Port)]).

:- http_handler('/api', handle, []).

read_term_from_atom([],[]). % need special case for empty list
read_term_from_atom(A,T) :- read_term_from_atom(A,T,[]).

handle(Request) :-
    http_read_json(Request, JSONIn),
    json_to_prolog(JSONIn, [Pred|Args]),
    maplist(read_term_from_atom, Args, ArgsTerms),
    member(Pred, [numSolutions, cardDefused, placeDice]),
    aggregate_all(set([Pred|ArgsTerms]), (Goal =.. [Pred|ArgsTerms], call(Goal)), Results),
    maplist(maplist(term_to_atom), Results, Output),
    prolog_to_json(Output, JSONOut),
    reply_json(JSONOut).

