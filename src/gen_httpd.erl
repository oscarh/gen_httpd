-module(gen_httpd).
-behaviour(gen_tcpd).

-export([start_link/4, start_link/5, recv/3]).
-export([init/1, handle_connection/2]).

-export([behaviour_info/1]).

-record(state, {
	socket,
	timeout,
	callback,
	callbackstate,
	buf
}).

start_link(Callback, CallbackArgs, Port, Timeout) ->
	IntState = #state{
		timeout = Timeout,
		callback = Callback,
		callbackstate = CallbackArgs,
		buf = []
	},
	gen_tcpd:start_link(?MODULE, IntState, tcp, Port, [{active, false}]).

start_link(Callback, CallbackArgs, Port, Timeout, SSL) ->
	IntState = #state{
		timeout = Timeout,
		callback = Callback,
		callbackstate = CallbackArgs,
		buf = []
	},
	gen_ssld:start_link(?MODULE, IntState, Port, [{active, false}|SSL]).

recv(#state{buf = Buf} = State, _T, Size) when length(Buf) >= Size ->
	{Data, NewBuf} = lists:split(Buf, Size),
	{Data, State#state{buf = NewBuf}};
recv(#state{socket = Socket, buf = Buf} = State, T, Size) ->
	case gen_tcpd:recv(Socket, Size - length(Buf), T) of
		{ok, Packet} when length(Buf) + length(Packet) == Size ->
			{Buf ++ Packet, State#state{buf = []}};
		{ok, Packet} ->
			recv(State#state{buf = Buf ++ Packet}, T, Size - length(Buf) - length(Packet));
		{error, timeout} ->
			exit(timeout);
		{error, Reason} ->
			exit(Reason)
	end.

init(#state{callback = Callback, callbackstate = CallbackArgs} = State) ->
	case Callback:init(CallbackArgs) of
		{ok, CallbackState} ->
			{ok, State#state{callbackstate = CallbackState}};
		Other ->
			exit({bad_return, Other})
	end.

% TODO supervise this prcocess since the TCP parent no longer cares.
% XXX This should MOST probably be done by the gen_tcpd supervisor though,
% since that is the one I used to start link :/ So, gen_tcpd supervisor
% should care about / supervise it's acceptors, which will become this
% process later. How does that work with receiving from sockets etc. 
handle_connection(Socket, State) ->
	NewState = State#state{socket = Socket},
	{Req, Headers, Body} = receive_loop(NewState, []),
	handle_request(NewState#state{buf = Body}, Req, Headers).

handle_request(State, {Method, Path, Vsn}, Headers) ->
	{Close, NewState} = case call_callback(State, Method, Path, Vsn, Headers) of
		{C, NS} ->
			{C == close, NS};
		{C, Reply, NS} ->
			ok = gen_tcpd:send(NS#state.socket, Reply),
			{C == close, NS}
	end,
	case {Vsn, lists:keysearch("Connection", 1, Headers), Close} of
		{"HTTP/1.1", {value, {_, KeepAlive}}, true} when KeepAlive /= "close" ->
			handle_connection(NewState#state.socket, State);
		_ ->
			gen_tcpd:close(NewState#state.socket),
			exit(normal)
	end.

receive_loop(#state{socket = Socket} = S, Acc) ->
	case gen_tcpd:recv(Socket, 0, S#state.timeout) of
		{ok, Packet} ->
			case catch gen_httpd_parser:parse(Acc ++ Packet) of
				{'EXIT', incomplete} ->
					receive_loop(S, Acc ++ Packet);
				Result ->
					Result
			end;
		{error, timeout} ->
			gen_tcpd:close(Socket),
			exit(normal)
	end.

call_callback(State, "GET", Path, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	{State, Callback:handle_get(Path, VSN, Headers, CBState)};
call_callback(State, "PUT", Path, VSN, Headers) ->
	{Body, NewState} = handle_upload(State, Headers),
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	{NewState, Callback:handle_put(Path, VSN, Headers, Body, CBState)};
call_callback(State, "HEAD", Path, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	{State, Callback:handle_head(Path, VSN, Headers, CBState)};
call_callback(State, "POST", Path, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	{Body, NewState} = handle_upload(State, Headers),
	{NewState, Callback:handle_post(Path, VSN, Headers, Body, CBState)};
call_callback(State, "OPTIONS", Path, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	Callback:handle_options(Path, VSN, Headers, CBState);
call_callback(State, "TRACE", Path, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	{State, Callback:handle_trace(Path, VSN, Headers, CBState)}.

handle_upload(State, Headers) ->
	case lists:keysearch("Content-Length", 1, Headers) of
		{value, {_, Size}} -> recv(State, State#state.timeout, list_to_integer(Size));
		false              -> {[], State}
	end.

behaviour_info(callbacks) ->
	[
		{init,1},
		{handle_get, 4},
		{handle_put, 5},
		{handle_head, 4},
		{handle_post, 5},
		{handle_options, 4},
		{handle_trace, 4}
	].
