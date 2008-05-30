-module(gen_httpd).
-export([start_link/4, start_link/5, recv/3]).
-export([init/1, handle_request/2]).

-record(state, {
	socket,
	sock_mod,
	timeout,
	mod,
	args,
	buf
}).

start_link(Port, Timeout, Mod, State) ->
	IntState = #state{
		sock_mod = gen_tcp,
		timeout = Timeout,
		mod = Mod,
		args = State,
		buf = []
	},
	gen_tcpd:start_link(?MODULE, IntState, Port, [{active, false}]).

start_link(Port, Timeout, SSL, Mod, State) ->
	IntState = #state{
		sock_mod = ssl,
		timeout = Timeout,
		mod = Mod,
		args = State,
		buf = []
	},
	gen_ssld:start_link(?MODULE, IntState, Port, [{active, false}|SSL]).

recv(#state{buf = Buf} = State, _T, Size) when length(Buf) >= Size ->
	{Data, NewBuf} = lists:split(Buf, Size),
	{Data, State#state{buf = NewBuf}};
recv(#state{socket = Socket, buf = Buf} = State, T, Size) ->
	case (State#state.sock_mod):recv(Socket, Size - length(Buf), T) of
		{ok, Packet} when length(Buf) + length(Packet) == Size ->
			{Buf ++ Packet, State#state{buf = []}};
		{ok, Packet} ->
			recv(State#state{buf = Buf ++ Packet}, T, Size - length(Buf) - length(Packet));
		{error, timeout} ->
			exit(timeout);
		{error, Reason} ->
			exit(Reason)
	end.

init(State) ->
	{ok, State}.

handle_request(Socket, State) ->
	NewState = State#state{socket = Socket},
	{Req, Headers, Body} = receive_loop(NewState, []),
	handle_request(NewState#state{buf = Body}, Req, Headers).

handle_request(State, {Method, Path, Vsn}, Headers) ->
	{Close, NewState} = case call_callback(State, Method, Path, Vsn, Headers) of
		{C, NS} ->
			{C == close, NS};
		{C, Reply, NS} ->
			ok = (NS#state.sock_mod):send(NS#state.socket, Reply),
			{C == close, NS}
	end,
	case {Vsn, lists:keysearch("Connection", 1, Headers), Close} of
		{"HTTP/1.1", {value, {_, KeepAlive}}, true} when KeepAlive /= "close" ->
			handle_request(NewState#state.socket, State);
		_ ->
			(NewState#state.sock_mod):close(NewState#state.socket),
			exit(normal)
	end.

receive_loop(#state{socket = Socket, sock_mod = SockMod, timeout = Timeout} = S, Acc) ->
	case SockMod:recv(Socket, 0, Timeout) of
		{ok, Packet} ->
			case catch gen_httpd_parser:parse(Acc ++ Packet) of
				{'EXIT', incomplete} ->
					receive_loop(S, Acc ++ Packet);
				Result ->
					Result
			end;
		{error, timeout} ->
			SockMod:close(Socket),
			exit(normal)
	end.

call_callback(State, "GET", Path, VSN, Headers) ->
	(State#state.mod):handle_get(State, {get, Path, VSN}, Headers, State#state.args);
call_callback(State, "PUT", Path, VSN, Headers) ->
	{Body, NewState} = handle_upload(State, Headers),
	(State#state.mod):handle_put(NewState, {put, Path, VSN}, Headers, State#state.args, Body);
call_callback(State, "HEAD", Path, VSN, Headers) ->
	(State#state.mod):handle_head(State, {head, Path, VSN}, Headers, State#state.args);
call_callback(State, "POST", Path, VSN, Headers) ->
	{Body, NewState} = handle_upload(State, Headers),
	(State#state.mod):handle_post(NewState, {post, Path, VSN}, Headers, State#state.args, Body);
call_callback(State, "OPTIONS", Path, VSN, Headers) ->
	(State#state.mod):handle_options(State, {options, Path, VSN}, Headers, State#state.args);
call_callback(State, "TRACE", Path, VSN, Headers) ->
	(State#state.mod):handle_trace(State, {trace, Path, VSN}, Headers, State#state.args).

handle_upload(State, Headers) ->
	case lists:keysearch("Content-Length", 1, Headers) of
		{value, {_, Size}} -> recv(State, State#state.timeout, list_to_integer(Size));
		false              -> {[], State}
	end.
