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

-include("gen_httpd.hrl").

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
	{Data, NewBuf} = lists:split(Size, Buf),
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

handle_request(State, {Method, URI, Vsn}, Headers) ->
	{NewState, Resp} = call_callback(State, Method, URI, Vsn, Headers),
	{Close, Reply, NewCBState} = handle_response(Vsn, Resp),
	ok = gen_tcpd:send(NewState#state.socket, Reply),
	case {Vsn, proplists:get_value("Connection", Headers), Close} of
		{"HTTP/1.1", KeepAlive, false} when KeepAlive /= "close" ->
			handle_request(NewState#state{callbackstate = NewCBState});
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
			exit(normal);
		{error, closed} ->
			gen_tcpd:close(Socket),
			exit(normal);
		{error, Reason} ->
			exit(Reason)
	end.

call_callback(State, "GET", URI, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	ConnInfo = conn_info(State#state.socket),
	{State, Callback:handle_get(URI, VSN, Headers, ConnInfo, CBState)};
call_callback(State, "PUT", URI, VSN, Headers) ->
	{Body, NewState} = handle_upload(State, Headers),
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	ConnInfo = conn_info(State#state.socket),
	Reply = Callback:handle_put(URI, VSN, Headers, Body, ConnInfo, CBState),
	{NewState, Reply};
call_callback(State, "HEAD", URI, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	ConnInfo = conn_info(State#state.socket),
	{State, Callback:handle_head(URI, VSN, Headers, ConnInfo, CBState)};
call_callback(State, "POST", URI, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	{Body, NewState} = handle_upload(State, Headers),
	ConnInfo = conn_info(State#state.socket),
	Reply = Callback:handle_post(URI, VSN, Headers, Body, ConnInfo, CBState),
	{NewState, Reply};
call_callback(State, "OPTIONS", URI, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	ConnInfo = conn_info(State#state.socket),
	Callback:handle_options(URI, VSN, Headers, ConnInfo, CBState);
call_callback(State, "TRACE", URI, VSN, Headers) ->
	Callback = State#state.callback,
	CBState = State#state.callbackstate,
	ConnInfo = conn_info(State#state.socket),
	{State, Callback:handle_trace(URI, VSN, Headers, ConnInfo, CBState)}.

handle_upload(State, Headers) ->
	case proplists:get_value("Content-Length", Headers) of
		undefined -> {[], State};
		Size      -> recv(State, State#state.timeout, list_to_integer(Size))
	end.

conn_info(Socket) ->
	{ok, {RemoteHost, RemotePort}} = gen_tcpd:peername(Socket),
	{ok, {LocalHost, LocalPort}} = gen_tcpd:sockname(Socket),
	Type = case gen_tcpd:type(Socket) of
		ssl -> https;
		tcp -> http
	end,
	#gen_httpd_conn{
		schema = Type,
		remote_address = RemoteHost,
		remote_port = RemotePort,
		local_address = LocalHost,
		local_port = LocalPort
	}.

handle_response(Vsn, {reply, Status, Reason, Headers, Close, CBState}) ->
	Resp = [
		status_line(Vsn, Status, Reason),
		headers(Vsn, Headers, Close)
	],
	{Close, Resp, CBState};
handle_response(Vsn, {reply, Status, Reason, Headers, Body, Close, CBState}) ->
	Resp = [
		status_line(Vsn, Status, Reason),
		headers(Vsn, add_headers(Headers, [], iolist_size(Body)), Close),
		Body
	],
	{Close, Resp, CBState}.

add_headers([{"Content-Length", _} | _] = Headers, Acc, _) ->
	Headers ++ Acc;
add_headers([{"content-length", _} | _] = Headers, Acc, _) ->
	Headers ++ Acc;
add_headers([{"Transfer-Encoding", _} | _] = Headers, Acc, _) ->
	Headers ++ Acc;
add_headers([{"transfer-encoding", _} | _] = Headers, Acc, _) ->
	Headers ++ Acc;
add_headers([H | T], Acc, ContentLength) ->
	add_headers(T, [H | Acc], ContentLength);
add_headers([], Acc, ContentLength) ->
	[{"Content-Length", integer_to_list(ContentLength)} | Acc].

status_line(Vsn, Status, Reason) ->
	[Vsn, $\ , integer_to_list(Status), $\ , Reason, $\r, $\n].

headers(Vsn, Headers, Close) ->
	AllHeaders = case {Vsn, Close} of
		{"HTTP/1.1", true} ->
			[{"Connection", "close"} | Headers];
		{_, _} ->
			Headers
	end,
	[lists:map(fun({Name, Value}) ->
					[Name, $:, $\ , Value, $\r, $\n]
			end, AllHeaders), $\r, $\n].
