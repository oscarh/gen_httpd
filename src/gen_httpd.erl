-module(gen_httpd).
-behaviour(gen_tcpd).

-export([start_link/4, start_link/5, recv/3]).
-export([init/1, handle_connection/2]).

-export([behaviour_info/1]).

-record(state, {
	callback,
	args,
	timeout
}).

-define(SOCKOPTS, {active, false}, {reuseaddr, true}).

start_link(Callback, CallbackArgs, Port, Timeout) ->
	Args = [Callback, CallbackArgs, Timeout],
	gen_tcpd:start_link(?MODULE, Args, tcp, Port, [?SOCKOPTS]).
	
start_link(Callback, CallbackArgs, Port, Timeout, SSL) ->
	Args = [Callback, CallbackArgs, Timeout],
	gen_tcpd:start_link(?MODULE, Args, ssl, Port, [?SOCKOPTS | SSL]).

recv(Opaque, T, Size) ->
	gen_httpd_handler:recv(Opaque, T, Size).

init([Callback, Args, Timeout]) ->
	{ok, #state{callback = Callback, args  = Args, timeout = Timeout}}.

handle_connection(Socket, State) ->
	Pid = gen_httpd_handler:spawn_handler(
		State#state.callback,
		State#state.args,
		State#state.timeout
	),
	gen_tcpd:controlling_process(Socket, Pid),
	gen_httpd_handler:handle_connection(Pid, Socket),
	{noreply, State}.

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
