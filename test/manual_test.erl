-module(manual_test).
-behaviour(gen_httpd).

-export([start/1]).

-export([
        init/2,
        handle_continue/5,
        handle_request/6,
        terminate/2
    ]).

start(Port) ->
    gen_httpd:start_link(?MODULE, nil, Port, 300000, []).

init(Socket, Arg) ->
    io:format("init(~p, ~p)~n", [Socket, Arg]),
    {ok, {Socket, Arg}}.

handle_continue(Method, URL, VSN, Headers,  State) ->
    io:format("handle_continue(~p, ~p, ~p, ~p, ~p)~n",
        [Method, URL, VSN, Headers, State]),
    {continue, [], State}.

handle_request(Method, URL, VSN, Headers, Body,  State) ->
    io:format("handle_request(~p, ~p, ~p, ~p, ~p, ~p)~n",
        [Method, URL, VSN, Headers, Body, State]),
	{reply, 200, [], <<"Yay!">>, State}.

terminate(Reason, State) ->
    io:format("terminate(~p, ~p)~n", [Reason, State]).
