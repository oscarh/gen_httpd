-module(gen_httpd).
-export([start_link/4, start_link/5]).
-export([init/1, handle_request/2]).

-record(state, {
	sock_mod,
	timeout,
	mod,
	args
}).

start_link(Port, Timeout, Mod, State) ->
	IntState = #state{sock_mod = gen_tcp, timeout = Timeout, mod = Mod, args = State},
	gen_tcpd:start_link(?MODULE, IntState, Port, [{active, false}]).

start_link(Port, Timeout, SSL, Mod, State) ->
	IntState = #state{sock_mod = ssl, timeout = Timeout, mod = Mod, args = State},
	gen_ssld:start_link(?MODULE, IntState, Port, [{active, false}|SSL]).

init(State) ->
	{ok, State}.

handle_request(Socket, State) ->
	{{_Method, _Path, Vsn} = Req, Headers, Body} =
		receive_loop(Socket, State#state.sock_mod, State#state.timeout, []),
	Response = call_callback(State, Socket, Req, Headers, Body),	
	ok = (State#state.sock_mod):send(Socket, Response),
	case {Vsn, lists:keysearch("Connection", 1, Headers)} of
		{"HTTP/1.1", {value, {_, KeepAlive}}} when KeepAlive /= "close" ->
			handle_request(Socket, State);
		_ ->
			(State#state.sock_mod):close(Socket),
			exit(normal)
	end.

receive_loop(Socket, SockMod, Timeout, Acc) ->
	case SockMod:recv(Socket, 0, Timeout) of
		{ok, Packet} ->
			case catch gen_httpd_parser:parse(Acc ++ Packet) of
				{'EXIT', incomplete} ->
					receive_loop(Socket, SockMod, Timeout, Acc ++ Packet);
				Result ->
					Result
			end;
		{error, timeout} ->
			SockMod:close(Socket),
			exit(normal)
	end.

call_callback(State, Socket, {"GET", Path, VSN}, Headers, _Body) ->
	(State#state.mod):handle_get(State#state.args, Socket, {get, Path, VSN}, Headers);
call_callback(State, Socket, {"PUT", Path, VSN}, Headers, Body) ->
	(State#state.mod):handle_put(State#state.args, Socket, {put, Path, VSN}, Headers, Body);
call_callback(State, Socket, {"HEAD", Path, VSN}, Headers, _Body) ->
	(State#state.mod):handle_head(State#state.args, Socket, {head, Path, VSN}, Headers);
call_callback(State, Socket, {"POST", Path, VSN}, Headers, Body) ->
	(State#state.mod):handle_post(State#state.args, Socket, {post, Path, VSN}, Headers, Body);
call_callback(State, Socket, {"OPTIONS", Path, VSN}, Headers, _Body) ->
	(State#state.mod):handle_options(State#state.args, Socket, {options, Path, VSN}, Headers);
call_callback(State, Socket, {"TRACE", Path, VSN}, Headers, _Body) ->
	(State#state.mod):handle_trace(State#state.args, Socket, {trace, Path, VSN}, Headers).

