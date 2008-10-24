-module(gen_httpd_handler).

-export([start/5]).

-export([read_pipeline/4, handle_async_request/6]).

-import(gen_httpd_util, [header_value/2, header_value/3, header_exists/2]).

-include("gen_httpd.hrl").

start(Callback, CallbackArgs, Socket, Timeout, Pipeline) ->
	CState = case Callback:init(CallbackArgs) of
		{ok, State} -> State;
		Other       -> exit({invalid_return_value, Other})
	end,
	if
		Pipeline > 1 ->
			pipeline_controller(Callback, CState, Socket, Timeout, Pipeline);
		Pipeline < 2 ->
			read_requests(Callback, CState, Socket, Timeout, [])
	end.

pipeline_controller(Callback, CState, Socket, Timeout, Pipeline) ->
	Info = conn_info(Socket),
	ReaderArgs = [self(), Socket, Timeout, []],
	Reader = spawn_link(?MODULE, read_pipeline, ReaderArgs),
	Queue = gen_httpd_pipeline_queue:new(Pipeline),
	pipeline_controller(Callback, CState, Socket, Info, Reader, Queue).

pipeline_controller(Callback, CState, Socket, Info, Reader, Queue0) ->
	QueueX = receive
		{request, Request} ->
			Queue1 = pipeline_request(Callback, CState, Request, Info, Queue0),
			case gen_httpd_pipeline_queue:is_full(Queue1) of
				false -> Reader ! next;
				true  -> ok
			end,
			Queue1;
		{response, Id, Response} ->
			Queue1 = pipeline_response(Id, Response, Socket, Queue0),
			case gen_httpd_pipeline_queue:is_full(Queue0) of
				true -> 
					case gen_httpd_pipeline_queue:is_full(Queue1) of
						false -> Reader ! next;
						true  -> ok
					end;
				false ->
					ok % Already waiting for request.
			end,
			Queue1;
		{error, bad_request} ->
			Response = bad_request(false),
			Id = gen_httpd_pipeline_queue:next_id(Queue0),
			Queue1 = gen_httpd_pipeline_queue:push(bad_request, Queue0),
			Queue2 = pipeline_response(Id, Response, Socket, Queue1),
			case gen_httpd_pipeline_queue:is_full(Queue2) of
				false -> Reader ! next;
				true  -> ok
			end,
			Queue2;
		{error, Reason} ->
			Callback:terminate(Reason, CState),
			exit(Reason);
		{'EXIT', Reader, Reason} ->
			Callback:terminate(Reason, CState),
			gen_tcpd:close(Socket),
			exit(Reason);
		{'EXIT', Pid, Reason} ->
			Report = [
				"Error in gen_httpd callback",
				{pid, Pid},
				{reason, Reason}
			],
			error_logger:error_report(Report),
			Response = internal_error("HTTP/1.1"),
			Id = gen_httpd_pipeline_queue:id(Pid, Queue0),
			pipeline_response(Id, Response, Socket, Queue0)
	end,
	pipeline_controller(Callback, CState, Socket, Info, Reader, QueueX).

pipeline_request(Callback, CState, Request, Info, Queue0) ->
	Id = gen_httpd_pipeline_queue:next_id(Queue0),
	Args = [self(), Id, Callback, CState, Info, Request],
	Pid = spawn_link(?MODULE, handle_async_request, Args),
	gen_httpd_pipeline_queue:push(Pid, Queue0).
	
pipeline_response(Id, Response, Socket, Queue0) ->
	{Responses, Queue1} =
		gen_httpd_pipeline_queue:response(Id, Response, Queue0),
	if
		length(Responses) =:= 0 ->
			ok;
		true ->
			lists:foreach(
				fun(Resp) -> gen_tcpd:send(Socket, Resp) end,
				Responses
			)
	end,
	Queue1.

read_pipeline(Handler, Socket, Timeout, Buff0) ->
	Buff2 = case receive_loop(Socket, Timeout, Buff0) of
		{ok, Req, Buff1} ->
			Handler ! {request, Req},
			Buff1;
		{error, Reason} ->
			Handler ! {error, Reason},
			[]
	end,
	receive
		next -> read_pipeline(Handler, Socket, Timeout, Buff2);
		stop -> exit(normal)
	end.

read_requests(Callback, CState0, Socket, Timeout, Buff0) ->
	case receive_loop(Socket, Timeout, Buff0) of
		{ok, Request, Buff1} ->
			ConnInfo = conn_info(Socket),
			case catch handle_request(Callback, CState0, ConnInfo, Request) of
				{continue, Response, CState1} ->
					ok = gen_tcpd:send(Socket, Response),
					read_requests(Callback, CState1, Socket, Timeout, Buff1);
				{stop, Reason, Response, CState1} ->
					ok = gen_tcpd:send(Socket, Response),
					Callback:terminate(Reason, CState1),
					gen_tcpd:close(Socket),
					exit(Reason);
				{'EXIT', Reason} ->
					Report = [
						"Error in gen_httpd callback",
						{pid, self()},
						{method, element(1, Request)},
						{uri, element(2, Request)},
						{version, element(3, Request)},
						{reason, Reason}
					],
					error_logger:error_report(Report),
					Response = internal_error(element(3, Request)),
					gen_tcpd:send(Socket, Response),
					exit(Reason)
			end;
		{error, bad_request = Reason} ->
			Callback:terminate(Reason, CState0),
			Response = bad_request(true),
			gen_tcpd:send(Socket, Response),
			gen_tcpd:close(Socket),
			exit(Reason);
		{error, Reason} ->
			Callback:terminate(Reason, CState0),
			gen_tcpd:close(Socket),
			exit(Reason)
	end.

receive_loop(_, Timeout, _) when Timeout < 0 ->
	{error, tcp_timeout};
receive_loop(Socket, Timeout, Buff0) ->
	Start = now(),
	case gen_tcpd:recv(Socket, 0, Timeout) of
		{ok, Packet} ->
			case catch gen_httpd_parser:parse(Buff0 ++ Packet) of
				{'EXIT', inclomplete} ->
					RTime = Timeout - timer:now_diff(now(), Start) div 1000,
					receive_loop(Socket, RTime, Buff0 ++ Packet);
				{'EXIT', _} ->
					{error, bad_request};
				{{"POST", URI, Vsn, Hdrs}, Buff1} ->
					RTime = Timeout - timer:now_diff(now(), Start) div 1000,
					case handle_upload(Socket, Buff1, Hdrs, RTime) of
						{ok, {Body, Buff2}} ->
							{ok, {"POST", URI, Vsn, Hdrs, Body}, Buff2};
						{error, Reason} ->
							{error, Reason}
					end;
				{{"PUT", URI, Vsn, Hdrs}, Buff1} ->
					RTime = Timeout - timer:now_diff(now(), Start) div 1000,
					case handle_upload(Socket, Buff1, Hdrs, RTime) of
						{ok, {Body, Buff2}} ->
							{ok, {"PUT", URI, Vsn, Hdrs, Body}, Buff2};
						{error, Reason} ->
							{error, Reason}
					end;
				{{Method, URI, Vsn, Hdrs}, Buff1} ->
					{ok, {Method, URI, Vsn, Hdrs, []}, Buff1}
			end;
		{error, timeout} ->
			{error, tcp_timeout};
		{error, Reason} ->
			{error, Reason}
	end.

handle_async_request(Pipeline, Id, Callback, CState, Info, Request) ->
	case handle_request(Callback, CState, Info, Request) of
		{continue, Response, _} ->
			Pipeline ! {response, Id, iolist_to_binary(Response)};
		{stop, _, Response, _} ->
			Pipeline ! {response, Id, iolist_to_binary(Response)}
	end.

handle_request(Callback, CState0, Info, {Method, URI, Vsn, Hdrs, Body}) ->
	Ret = call_cb(Callback, CState0, Info, Method, URI, Vsn, Hdrs, Body),
	{Response, CState1} = handle_cb_ret(Vsn, Ret),
	Connection = header_value("connection", Hdrs, undefined),
	case {Vsn, string:to_lower(Connection)} of
		{"HTTP/1.1", "close"} ->
			{stop, normal, Response, CState1};
		{"HTTP/1.1", _} ->
			{continue, Response, CState1};
		{"HTTP/1.0", "keep-alive"} ->
			{continue, Response, CState1};
		{_, _} ->
			{stop, normal, Response, CState1}
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

call_cb(Callback, CState, ConnInfo, "GET", URI, Vsn, Hdrs, _) ->
	Callback:handle_get(URI, Vsn, Hdrs, ConnInfo, CState);
call_cb(Callback, CState, ConnInfo, "HEAD", URI, Vsn, Hdrs, _) ->
	Callback:handle_head(URI, Vsn, Hdrs, ConnInfo, CState);
call_cb(Callback, CState, ConnInfo, "OPTIONS", URI, Vsn, Hdrs, _) ->
	Callback:handle_options(URI, Vsn, Hdrs, ConnInfo, CState);
call_cb(Callback, CState, ConnInfo, "TRACE", URI, Vsn, Hdrs, _) ->
	Callback:handle_trace(URI, Vsn, Hdrs, ConnInfo, CState);
call_cb(Callback, CState, ConnInfo, "PUT", URI, Vsn, Hdrs, Body) ->
	Callback:handle_put(URI, Vsn, Hdrs, Body, ConnInfo, CState);
call_cb(Callback, CState, ConnInfo, "POST", URI, Vsn, Hdrs, Body) ->
	Callback:handle_post(URI, Vsn, Hdrs, Body, ConnInfo, CState);
call_cb(Callback, CState, ConnInfo, "CONNECT", URI, Vsn, Hdrs, Body) ->
	Callback:handle_connect(URI, Vsn, Hdrs, Body, ConnInfo, CState).

handle_cb_ret(Vsn, {reply, Status, Headers0, Body, CState}) ->
	Headers1 = case header_exists("content-length", Headers0) of
		false ->
			Length = iolist_size(Body),
			[{"content-length", integer_to_list(Length)} | Headers0];
		true ->
			Headers0
	end,
	{[status_line(Vsn, Status), format_headers(Headers1), Body], CState};
handle_cb_ret(_, Return) ->
	exit({invalid_return_value, Return}).

handle_upload(Socket, Buff0, Hdrs, Timeout) ->
	case header_value("content-length", Hdrs) of
		undefined ->
			{[], Buff0};
		ContentLength ->
			case catch list_to_integer(ContentLength) of
				{'EXIT', _} ->
					{[], Buff0};
				Value ->
					get_body(Socket, Buff0, Value, Timeout)
			end
	end.

get_body(_, _, _, Timeout) when Timeout < 0 ->
	{error, Timeout};
get_body(Socket, Buff0, Length, Timeout) ->
	Start = now(),
	BuffLength = length(Buff0),
	if
		BuffLength < Length ->
			case gen_tcpd:recv(Socket, 0, Timeout) of
				{ok, Packet} ->
					RTime = Timeout - timer:now_diff(now(), Start) div 1000,
					get_body(Socket, Buff0 ++ Packet, Length, RTime);
				{error, timeout} ->
					{error, tcp_timeout};
				{error, Reason} ->
					{error, Reason}
			end;
		BuffLength >= Length ->
			{ok, lists:split(Length, Buff0)}
	end.

internal_error(Version) ->
	Headers = [{"connection", "close"}],
	[status_line(Version, 500), format_headers(Headers)].

bad_request(true) ->
	bad_request([{"connection", "close"}]);
bad_request(false) ->
	bad_request([]);
bad_request(Headers) ->
	[status_line("HTTP/1.1", 400), format_headers(Headers)].

reason(200) -> "OK";
reason(400) -> "Bad Request";
reason(500) -> "Internal server error".

status_line(Vsn, {Status, Reason}) ->
	[Vsn, $\ , integer_to_list(Status), $\ , Reason, $\r, $\n];
status_line(Vsn, Status) ->
	status_line(Vsn, {Status, reason(Status)}).

format_headers(Headers) ->
	[lists:map(fun({Name, Value}) ->
					[Name, $:, $\ , Value, $\r, $\n]
			end, Headers), $\r, $\n].
