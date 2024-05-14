-module(hello_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
    {ok, P} = python:start(),
    Diff = python:call(P, 'python.diff_logic', 'main', [<<".">>, <<"HEAD^">>]),

    Req_1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/plain">>},
        Diff,
        Req
    ),
    {ok, Req, State}.
