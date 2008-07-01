-module(gen_httpd_handler).

-export([spawn_handler/3, recv/3]).
-export([handle_connection/2]).

-record(state, {
	socket,
	timeout,
	callback,
	callbackstate,
	buf = []
}).

spawn_handler(Callback, Args, Timeout) ->
	proc_lib:spawn_link(fun() ->
		{ok, CState} = Callback:init(Args),
		receive
			Socket ->
				State = #state{
					socket = Socket,
					callback = Callback,
					callbackstate = CState,
					timeout = Timeout
				},
				case catch handle_request(State) of
					{'EXIT', Reason} ->
						Callback:terminate(Reason, CState);
					{_, #state{callbackstate =  CState0}} ->
						Callback:terminate(normal, CState0)
				end
		end
	end).

handle_connection(Handler, Socket) ->
	Handler ! Socket,
	ok.

recv(#state{buf = Buf} = State, _T, Size) when length(Buf) >= Size ->
	{Data, NewBuf} = lists:split(Buf, Size),
	{Data, State#state{buf = NewBuf}};
recv(#state{socket = Socket, buf = Buf} = State, T, Size) ->
	case gen_tcpd:recv(Socket, Size - length(Buf), T) of
		{ok, Packet} when length(Buf) + length(Packet) == Size ->
			{Buf ++ Packet, State#state{buf = []}};
		{ok, Packet} ->
			recv(State#state{buf = Buf ++ Packet}, T,
				Size - length(Buf) - length(Packet));
		{error, timeout} ->
			exit(timeout);
		{error, Reason} ->
			exit(Reason)
	end.

handle_request(State) ->
	{Req, Headers, Body} = receive_loop(State, []),
	handle_request(State#state{buf = Body}, Req, Headers).

handle_request(State, {Method, Path, Vsn}, Headers) ->
	{Close, NewState} =
	case call_callback(State, Method, Path, Vsn, Headers) of
		{C, NS} ->
			{C == close, NS};
		{C, Reply, NS} ->
			ok = gen_tcpd:send(NS#state.socket, Reply),
			{C == close, NS}
	end,
	case {Vsn, proplists:get_value("Connection", Headers), Close} of
		{"HTTP/1.1", KeepAlive, true} when KeepAlive /= "close" ->
			handle_request(State);
		_ ->
			gen_tcpd:close(NewState#state.socket),
			{ok, NewState}
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
	case proplists:get_value("Content-Length", Headers) of
		undefined -> {[], State};
		Size      -> recv(State, State#state.timeout, list_to_integer(Size))
	end.
