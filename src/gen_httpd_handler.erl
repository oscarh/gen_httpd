%%% ----------------------------------------------------------------------------
%%% Copyright 2008
%%% Martin Carlson, martin@martinc.eu
%%% Oscar Hellström, oscar@hellstrom.st
%%%
%%% All rights reserved
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in the
%%%       documentation and/or other materials provided with the distribution.
%%%     * The names of its contributors may not be used to endorse or promote
%%%       products derived from this software without specific prior written
%%%       permission.
%%%
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%% ----------------------------------------------------------------------------
%%% @copyright 2008 Martin Carlson, Oscar Hellström
%%% @author Martin Carlson <martin@martinc.eu>
%%% @author Oscar Hellström <oscar@hellstrom.st> [http://oscar.hellstrom.st]
%%% @version {@version}, {@date}, {@time}
%%% @private
%%% @doc
%%% 
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd_handler).

-export([start/5]).
-export([pipeline_reader/5, handle_async_request/5]).

-import(gen_httpd_util, [header_value/2, header_value/3]).

-include("gen_httpd.hrl").

start(Callback, CallbackArgs, Socket, Timeout, Pipeline) ->
	process_flag(trap_exit, true),
	Info = conn_info(Socket),
	CState = case Callback:init(Info, CallbackArgs) of
		{ok, State} -> State;
		Other       -> exit({invalid_return_value, Other})
	end,
	if
		Pipeline > 1 ->
			start_pipeline(Callback, CState, Socket, Timeout, Pipeline);
		Pipeline < 2 ->
			request_reader(Callback, CState, Socket, Timeout)
	end.

pipeline_reader(Handler, Socket, Callback, CState, Timeout) ->
	case receive_loop(Socket, Callback, CState, Timeout) of
		{ok, Req, _} ->
			Handler ! {request, Req};
		{error, bad_request} ->
			Handler ! {request, bad_request};
		{error, Reason} ->
			exit(Reason)
	end,
	receive
		next -> pipeline_reader(Handler, Socket, Callback, CState, Timeout)
	end.

start_pipeline(Callback, CState, Socket, Timeout, Pipeline) ->
	ReaderArgs = [self(), Socket, Callback, CState, Timeout],
	Reader = spawn_link(?MODULE, pipeline_reader, ReaderArgs),
	Queue = gen_httpd_pipeline_queue:new(Pipeline),
	pipeline_controller(Callback, CState, Socket, Reader, Queue).

pipeline_controller(CB, CState, Socket, Reader, Queue0) ->
	QueueX = receive
		{response, Id, Resp, _, KeepAlive} ->
			case pipeline_response(Id, Resp, KeepAlive, Socket, Queue0) of
				{ok, Queue1} ->
					WasFull = gen_httpd_pipeline_queue:is_full(Queue0),
					IsFull = gen_httpd_pipeline_queue:is_full(Queue1),
					if
						WasFull andalso not IsFull ->
							Reader ! next;
						not WasFull orelse IsFull ->
							ok
					end,
					Queue1;
				stop ->
					unlink(Reader),
					exit(Reader, kill),
					gen_tcpd:close(Socket),
					exit(normal)
			end;
		{request, bad_request} ->
			Response = gen_httpd_util:bad_request_resp(false),
			Id = gen_httpd_pipeline_queue:next_id(Queue0),
			Queue1 = gen_httpd_pipeline_queue:push(bad_request, Queue0),
			case pipeline_response(Id, Response, false, Socket, Queue1) of
				{ok, Queue2} ->
					Queue2;
				stop ->
					unlink(Reader),
					exit(Reader, kill),
					gen_tcpd:close(Socket),
					exit(normal)
			end;
		{request, Request} ->
			Queue1 = pipeline_request(CB, CState, Request, Queue0),
			case gen_httpd_pipeline_queue:is_full(Queue1) of
				false -> Reader ! next;
				true  -> ok
			end,
			Queue1;
		{'EXIT', Reader, Reason} ->
			gen_tcpd:close(Socket),
			CB:terminate(closed, CState),
			exit(Reason);
		{'EXIT', _, normal} ->
			Queue0;
		{'EXIT', Pid, Reason} ->
			Report = [
				"Error in gen_httpd callback",
				{pid, Pid},
				{reason, Reason}
			],
			error_logger:error_report(Report),
			Resp = gen_httpd_util:internal_error_resp({1, 1}),
			Id = gen_httpd_pipeline_queue:id(Pid, Queue0),
			pipeline_response(Id, Resp, false, Socket, Queue0),
			unlink(Reader),
			exit(Reader, kill),
			gen_tcpd:close(Socket),
			exit(normal)
	end,
	pipeline_controller(CB, CState, Socket, Reader, QueueX).

request_reader(Callback, CState0, Socket, Timeout) ->
	case receive_loop(Socket, Callback, CState0, Timeout) of
		{ok, Request, CState1} ->
			Args = [self(), nil, Callback, CState1, Request],
			Pid = spawn_link(?MODULE, handle_async_request, Args),
			receive
				{response, _, Response, CState2, true} ->
					ok = gen_tcpd:send(Socket, Response),
					request_reader(Callback, CState2, Socket, Timeout);
				{response, _, Response, CState2, false} ->
					ok = gen_tcpd:send(Socket, Response),
					Callback:terminate(normal, CState2),
					gen_tcpd:close(Socket),
					exit(normal);
				{'EXIT', Pid, Reason} ->
					Vsn = element(3, Request),
					Response = gen_httpd_util:internal_error_resp(Vsn),
					ok = gen_tcpd:send(Socket, Response),
					gen_tcpd:close(Socket),
					Report = [
						"Error in gen_httpd callback",
						{pid, self()},
						{method, element(1, Request)},
						{uri, element(2, Request)},
						{version, Vsn},
						{reason, Reason}
					],
					error_logger:error_report(Report),
					exit(normal);
				{'EXIT', AnotherPid, Reason} ->
					Report = [
						"Error in process linked to gen_httpd_handler",
						{pid, AnotherPid},
						{reason, Reason}
					],
					error_logger:error_report(Report),
					exit(Reason)
			end;
		{error, bad_request} ->
			Response = gen_httpd_util:bad_request_resp(true),
			gen_tcpd:send(Socket, Response),
			gen_tcpd:close(Socket),
			Callback:terminate(bad_request, CState0),
			exit(bad_request);
		{error, tcp_timeout} ->
			gen_tcpd:close(Socket),
			Callback:terminate(tcp_timeout, CState0),
			exit(tcp_timeout);
		{error, Reason} ->
			gen_tcpd:close(Socket),
			Callback:terminate(Reason, CState0),
			exit(Reason)
	end.

pipeline_request(Callback, CState, Request, Queue0) ->
	Id = gen_httpd_pipeline_queue:next_id(Queue0),
	Args = [self(), Id, Callback, CState, Request],
	Pid = spawn_link(?MODULE, handle_async_request, Args),
	gen_httpd_pipeline_queue:push(Pid, Queue0).
	
pipeline_response(Id, Response, KeepAlive, Socket, Queue0) ->
	{Responses, Queue1} =
		gen_httpd_pipeline_queue:response(Id, {Response, KeepAlive}, Queue0),
	case send_responses(Responses, Socket) of
		ok -> {ok, Queue1};
		stop -> stop
	end.

send_responses([{Resp, true} | T], Socket) ->
	gen_tcpd:send(Socket, Resp),
	send_responses(T, Socket);
send_responses([{Resp, false} | _], Socket) ->
	gen_tcpd:send(Socket, Resp),
	stop;
send_responses([], _) ->
	ok.

receive_loop(_, _, _, Timeout) when Timeout < 0 ->
	{error, tcp_timeout};
receive_loop(Socket, Callback, CState0, Timeout) ->
	Start = now(),
	ok = gen_tcpd:setopts(Socket, [{packet, http}]),
	case http_packet(Socket, Timeout, nil, nil, nil, []) of
		{ok, {Method, URI, Vsn, Hdrs} = Request} ->
			RTime = Timeout - timer:now_diff(now(), Start) div 1000,
			case get_body(Socket, Callback, CState0, Request, RTime) of
				{ok, Body, CState1} ->
					{ok, {Method, URI, Vsn, Hdrs, Body}, CState1};
				{error, Reason} ->
					{error, Reason};
				{continue, CState1} ->
					receive_loop(Socket, Callback, CState1, Timeout)
			end;
		{error, timeout} ->
			{error, tcp_timeout};
		{error, Reason} ->
			{error, Reason}
	end.

http_packet(_, Timeout, _, _, _, _) when Timeout < 0 ->
	{error, tcp_timeout};
http_packet(Socket, Timeout, Method0, URI0, Vsn0, Hdrs0) ->
	Start = now(),
	case gen_tcpd:recv(Socket, 0, Timeout) of
		{ok, {http_request, Method1, URI1, Vsn1}} ->
			RTime = Timeout - timer:now_diff(now(), Start) div 1000,
			http_packet(Socket, RTime, Method1, URI1, Vsn1, Hdrs0);
		{ok, {http_header, _, Name, _, Value}} when is_atom(Name) ->
			RTime = Timeout - timer:now_diff(now(), Start) div 1000,
			Hdrs1 = [{atom_to_list(Name), Value} | Hdrs0],
			http_packet(Socket, RTime, Method0, URI0, Vsn0, Hdrs1);
		{ok, {http_header, _, Name, _, Value}} when is_list(Name) ->
			RTime = Timeout - timer:now_diff(now(), Start) div 1000,
			Hdrs1 = [{Name, Value} | Hdrs0],
			http_packet(Socket, RTime, Method0, URI0, Vsn0, Hdrs1);
		{ok, http_eoh} ->
			{ok, {Method0, URI0, Vsn0, Hdrs0}};
		{error, {http_error, _}} ->
			{error, bad_request};
		{error, Reason} ->
			{error, Reason}
	end.

get_body(Socket, Callback, CState, {'POST', _, _, _} = Request, Timeout) ->
	do_get_body(Socket, Callback, CState, Request, Timeout);
get_body(Socket, Callback, CState, {'PUT', _, _, _} = Request, Timeout) ->
	do_get_body(Socket, Callback, CState, Request, Timeout);
get_body(_, _, CState, _, _) ->
	{ok, <<>>, CState}.

do_get_body(Socket, Callback, CState0, R0, Timeout) ->
	Hdrs = element(4, R0),
	case handle_upload(Socket, Hdrs, Timeout) of
		{ok, Body} ->
			{ok, Body, CState0};
		expect_continue ->
			case handle_continue(Socket, Callback, CState0, R0) of
				{get_body, R1, CState1} ->
					do_get_body(Socket, Callback, CState1, R1, Timeout);
				{continue, CState1} ->
					{continue, CState1}
			end;
		{error, timeout} ->
			{error, tcp_timeout};
		{error, Reason} ->
			{error, Reason}
	end.

handle_continue(Socket, CB, CState0, {Method, URI, Vsn, Hdrs0}) ->
	case catch CB:handle_continue(Method, URI, Vsn, Hdrs0, CState0) of
		{continue, CState1} ->
			Response = gen_httpd_util:continue_resp(Vsn),
			ok = gen_tcpd:send(Socket, Response),
			Hdrs1 = gen_httpd_util:remove_header("expect", Hdrs0),
			Request = {'POST', URI, Vsn, Hdrs1},
			{get_body, Request, CState1};
		{'EXIT', Reason} ->
			{error, Reason};
		Reply ->
			{Resp, _, CState1} = handle_cb_ret('POST', Vsn, Reply, undefined),
			ok = gen_tcpd:send(Socket, Resp),
			{continue, CState1}
	end.

handle_async_request(Controller, Id, CB, CState0, Request) ->
	{Resp, CState1, KeepAlive} = handle_request(CB, CState0, Request),
	Controller ! {response, Id, Resp, CState1, KeepAlive},
	%% Unlink from controller, to avoid having to deal with the exit
	%% message.
	unlink(Controller).

handle_request(Callback, CState0, {Method, URI, Vsn, Hdrs, Body}) ->
	Ret = call_cb(Callback, CState0, Method, URI, Vsn, Hdrs, Body),
	ClConn = case header_value("connection", Hdrs, "") of
		undefined -> undefined;
		String    -> string:to_lower(String)
	end,
	handle_cb_ret(Method, Vsn, Ret, ClConn).

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

call_cb(CB, CState, Method, URI, Vsn, Hdrs, Body) ->
	case catch CB:handle_request(Method, URI, Vsn, Hdrs, Body, CState) of
		{'EXIT', {function_clause, [{CB, handle_request, _} | _]}} ->
			{reply, 501, [{"Connection", "close"}], [], CState};
		{'EXIT', Reason} ->
			Report = [
				"Error in gen_httpd callback",
				{pid, self()},
				{method, Method},
				{uri, URI},
				{version, Vsn},
				{reason, Reason}
			],
			error_logger:error_report(Report),
			{reply, 500, [{"Connection", "close"}], [], CState};
		Reply ->
			Reply
	end.

handle_cb_ret(Method, Vsn, {reply, Status, Hdrs0, Body, CState}, ClConn) ->
	Hdrs1 = case gen_httpd_util:header_exists("content-length", Hdrs0) of
		false -> add_content_length(Method, Status, Hdrs0, iolist_size(Body));
		true  -> Hdrs0
	end,
	{_, Minor} = Vsn,
	{Hdrs2, KeepAlive} = case header_value("connection", Hdrs1) of
		undefined ->
			case ClConn of
				"close" ->
					{[{"Connection", "close"} | Hdrs1], false};
				"keep-alive" when Minor =:= 0 ->
					{[{"Connection", "Keep-Alive"} | Hdrs1], true};
				_ when Minor > 0 ->
					{Hdrs1, true};
				_ when Minor =:= 0 ->
					{Hdrs1, false}
			end;
		"close" ->
			{Hdrs1, false};
		_ ->
			case ClConn of
				"close"            -> {Hdrs1, false};
				"keep-alive"       -> {Hdrs1, true};
				_ when Minor > 0   -> {Hdrs1, true};
				_ when Minor =:= 0 -> {Hdrs1, false}
			end
	end,
	Response = [
		gen_httpd_util:status_line(Vsn, Status),
		gen_httpd_util:format_headers(Hdrs2),
		Body
	],
	{Response, CState, KeepAlive};
handle_cb_ret(_, _, Return, _) ->
	exit({invalid_return_value, Return}).

%% Add content-length if it is required, even when it is 0
add_content_length('HEAD', _, Headers, _) ->
	Headers;
add_content_length(_, 200, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, {200, _}, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, 202, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, {202, _}, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, 203, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, {203, _}, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, 206, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, {206, _}, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, _, Headers, 0) ->
	Headers;
add_content_length(_, _, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers].

handle_upload(Socket, Hdrs, Timeout) ->
	case header_value("expect", Hdrs) of
		"100-continue" ->
			expect_continue;
		_ ->
			ContentLength = header_value("content-length", Hdrs),
			TransEnc = header_value("transfer-encoding", Hdrs, "identity"),
			case string:to_lower(TransEnc) of
				"identity" ->
					case catch list_to_integer(ContentLength) of
						{'EXIT', _} ->
							exit(bad_request);
						Length ->
							gen_tcpd:setopts(Socket, [{packet, raw}]),
							gen_tcpd:recv(Socket, Length, Timeout)
					end;
				"chunked" ->
					{ok, {chunked, fun() -> read_chunk(Socket, Timeout) end}};
				_Other ->
					exit(bad_request)
			end
	end.

read_chunk(Socket, Timeout) ->
	ok = gen_tcpd:setopts(Socket, [{packet, line}]),
	case gen_tcpd:recv(Socket, 0, Timeout) of
		{ok, Line} ->
			[ChunkSize | _] = string:tokens(binary_to_list(Line), ";\r\n"),
			ok = gen_tcpd:setopts(Socket, [{packet, raw}]),
			case  catch erlang:list_to_integer(ChunkSize, 16) of
				{'EXIT', _} -> {error, bad_request};
				0           -> {error, last_chunk};
				Size        -> gen_tcpd:recv(Socket, Size, Timeout)
			end;
		Other ->
			Other
	end.
