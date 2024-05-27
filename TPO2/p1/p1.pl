:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- http_handler(root(.), say_hi, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-
    format('Content-type: text/html~n~n'),
    format('<h1>Hello, World!<h1>').

