%%% ----------------------------------------------------------------------------
%%% Copyright 2008
%%% Martin Carlson, martin@erlang-consulting.com
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
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @author Oscar Hellström <oscar@hellstrom.st> [http://oscar.hellstrom.st]
%%% @version {@version}, {@date}, {@time}
%%% @doc
%%% 
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd_handler).

-export([start/5]).

-export([read_pipeline/4, handle_async_request/6]).

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
			Response = gen_httpd_util:bad_request_resp(false),
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
			Response = gen_httpd_util:internal_error_resp("HTTP/1.1"),
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
					Vsn = element(3, Request),
					Response = gen_httpd_util:internal_error_resp(Vsn),
					gen_tcpd:send(Socket, Response),
					exit(Reason)
			end;
		{error, bad_request = Reason} ->
			Callback:terminate(Reason, CState0),
			Response = gen_httpd_util:bad_request_resp(true),
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
	Connection = gen_httpd_util:header_value("connection", Hdrs, undefined),
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
	Headers1 = case gen_httpd_util:header_exists("content-length", Headers0) of
		false ->
			Length = iolist_size(Body),
			[{"content-length", integer_to_list(Length)} | Headers0];
		true ->
			Headers0
	end,
	Response = [
		gen_httpd_util:status_line(Vsn, Status),
		gen_httpd_util:format_headers(Headers1),
		Body
	],
	{Response, CState};
handle_cb_ret(_, Return) ->
	exit({invalid_return_value, Return}).

handle_upload(Socket, Buff0, Hdrs, Timeout) ->
	case gen_httpd_util:header_value("content-length", Hdrs) of
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
