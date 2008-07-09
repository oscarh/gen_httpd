-module(gen_httpd).
-behaviour(gen_tcpd).

-export([start_link/1, start_link/2, recv/3]).
-export([init/1, handle_connection/2, handle_info/2, terminate/2]).

-export([behaviour_info/1]).

-define(SOCKOPTS, {active, false}, {reuseaddr, true}).

start_link(Port) ->
	gen_tcpd:start_link(?MODULE, [], tcp, Port, [?SOCKOPTS]).
	
start_link(Port, SSL) ->
	gen_tcpd:start_link(?MODULE, [], ssl, Port, [?SOCKOPTS | SSL]).

recv(Opaque, T, Size) ->
	gen_httpd_handler:recv(Opaque, T, Size).

init(_) ->
	{ok, nil}.

handle_connection(Socket, State) ->
	{ok, Pid} = gen_httpd_handler_sup:spawn_handler(),
	gen_tcpd:controlling_process(Socket, Pid),
	gen_httpd_handler:handle_connection(Pid, Socket),
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

behaviour_info(callbacks) ->
	[
		{init,1},
		{handle_get, 5},
		{handle_put, 6},
		{handle_head, 5},
		{handle_post, 6},
		{handle_options, 5},
		{handle_trace, 5}
	].
