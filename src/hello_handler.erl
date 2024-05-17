-module(hello_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
    {ok, P} = python:start(),
    Diff = python:call(P, 'python_scripts.diff_logic', 'get_diff_json_from_args', [<<".">>, <<"diff-logic-refactor">>, <<"main">>]),

    Req_1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/plain">>},
        Diff,
        Req
    ),
    {ok, Req, State}.
