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
%%% @private
%%% @doc
%%% 
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd_handler).

-export([start/5]).

-export([read_pipeline/3, handle_async_request/6]).

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
			read_requests(Callback, CState, Socket, Timeout)
	end.

pipeline_controller(Callback, CState, Socket, Timeout, Pipeline) ->
	Info = conn_info(Socket),
	ReaderArgs = [self(), Socket, Timeout],
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
			Response = gen_httpd_util:internal_error_resp({1, 1}),
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

read_pipeline(Handler, Socket, Timeout) ->
	case receive_loop(Socket, Timeout) of
		{ok, Req} ->
			Handler ! {request, Req};
		{error, Reason} ->
			Handler ! {error, Reason},
			[]
	end,
	receive
		next -> read_pipeline(Handler, Socket, Timeout);
		stop -> exit(normal)
	end.

read_requests(Callback, CState0, Socket, Timeout) ->
	case receive_loop(Socket, Timeout) of
		{ok, Request} ->
			ConnInfo = conn_info(Socket),
			case catch handle_request(Callback, CState0, ConnInfo, Request) of
				{continue, Response, CState1} ->
					ok = gen_tcpd:send(Socket, Response),
					read_requests(Callback, CState1, Socket, Timeout);
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

receive_loop(_, Timeout) when Timeout < 1 ->
	{error, tcp_timeout};
receive_loop(Socket, Timeout) ->
	Start = now(),
	ok = gen_tcpd:setopts(Socket, [{packet, http}]),
	case http_packet(Socket, Timeout, nil, nil, nil, []) of
		{ok, {Method, URI, Vsn, Hdrs}} ->
			if
				Method =:= 'POST'; Method =:= 'PUT' ->
					RTime = Timeout - timer:now_diff(now(), Start) div 1000,
					case handle_upload(Socket, Hdrs, RTime) of
						{ok, Body} ->
							{ok, {Method, URI, Vsn, Hdrs, Body}};
						{error, Reason} ->
							{error, Reason}
					end;
				true ->
					{ok, {Method, URI, Vsn, Hdrs, <<>>}}
			end;
		{error, timeout} ->
			{error, tcp_timeout};
		{error, Reason} ->
			{error, Reason}
	end.

http_packet(_, Timeout, _, _, _, _) when Timeout < 1 ->
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

handle_async_request(Pipeline, Id, Callback, CState, Info, Request) ->
	case handle_request(Callback, CState, Info, Request) of
		{continue, Response, _} ->
			Pipeline ! {response, Id, iolist_to_binary(Response)};
		{stop, _, Response, _} ->
			Pipeline ! {response, Id, iolist_to_binary(Response)}
	end.

handle_request(Callback, CState0, Info, {Method, URI, Vsn, Hdrs, Body}) ->
	Ret = call_cb(Callback, CState0, Info, Method, URI, Vsn, Hdrs, Body),
	{Response, CState1} = handle_cb_ret(Method, Vsn, Ret),
	Connection = gen_httpd_util:header_value("connection", Hdrs, undefined),
	case {Vsn, string:to_lower(Connection)} of
		{{1, 1}, "close"} ->
			{stop, normal, Response, CState1};
		{{1, 1}, _} ->
			{continue, Response, CState1};
		{{1, 0}, "keep-alive"} ->
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

call_cb(CB, CState, Info, Method, URI, Vsn, Hdrs, Body) ->
	case catch CB:handle_request(Method, URI, Vsn, Hdrs, Body, Info, CState) of
		{'EXIT', {function_clause, [{CB, handle_request, _} | _]}} ->
			{reply, 501, [{"connection", "close"}], [], CState};
		Reply ->
			Reply
	end.

handle_cb_ret(Method, Vsn, {reply, Status, Headers0, Body, CState}) ->
	Headers1 = case gen_httpd_util:header_exists("content-length", Headers0) of
		false ->
			add_content_length(Method, Status, Headers0, iolist_size(Body));
		true ->
			Headers0
	end,
	Response = [
		gen_httpd_util:status_line(Vsn, Status),
		gen_httpd_util:format_headers(Headers1),
		Body
	],
	{Response, CState};
handle_cb_ret(_, _, Return) ->
	exit({invalid_return_value, Return}).

%% Add content-length if it is required, even when it is 0
add_content_length('HEAD', _, Headers, _) ->
	Headers;
add_content_length(_, 200, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, 202, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, 203, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, 206, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers];
add_content_length(_, _, Headers, 0) ->
	Headers;
add_content_length(_, _, Headers, Length) ->
	[{"content-length", integer_to_list(Length)} | Headers].

handle_upload(Socket, Hdrs, Timeout) ->
	case gen_httpd_util:header_value("content-length", Hdrs) of
		undefined ->
			{ok, <<"">>}; % FIXME return need length error
		ContentLength ->
			case catch list_to_integer(ContentLength) of
				{'EXIT', _} ->
					{ok, <<"">>};
				Length ->
					gen_tcpd:setopts(Socket, [{package, raw}]),
					gen_tcpd:recv(Socket, Length, Timeout)
			end
	end.
