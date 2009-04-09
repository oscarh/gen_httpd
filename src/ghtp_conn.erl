-module(ghttpd_conn).
-export([init/6]).

-record(ghtp_conn, {
		parent,
		reader,
		reader_state = active,
		callback,
		callback_state,
		socket,
		sock_timeout,
		pipeline_queue
	}).

-include("gen_httpd_int.hrl").

init(Parent, Callback, CallbackArg, Socket, SockTimeout, PipelineLength) ->
	CallbackState = case init_callback(Callback, CallbackArg, Socket) of
		{ok, S}        -> S;
		{stop, Reason} -> exit(Reason)
	end,
	State = #ghtp_conn{
		parent = Parent,
		callback = Callback,
		callback_state = CallbackState,
		socket = Socket,
		sock_timeout = SockTimeout,
		pipeline_queue = ghtp_pl_queue:new(PipelineLength)
	},
	process_flag(trap_exit, true),
	Reader = spawn_link(ghtp_reader, start, [self(), Socket]),
	loop(State#ghtp_conn{reader = Reader, callback_state = CBState}).

init_callback(Callback, CallbackArg, Socket) ->
	{ok, {RemoteAddrs, RemotePort}} = gen_tcpd:peername(Socket),
	{ok, {LocalAddrs, LocalPort}} = gen_tcpd:sockname(Socket),
	ConnInfo = #ghtp_conn{
		remote_address = RemoteAddrs,
		remote_port =  RemotePort,
		local_address = LocalAddrs,
		local_port = LocalPort
	},
	Callback:init(ConnInfo, CallbackArg).

loop(#ghtp_conn{reader = Reader} = State) ->
	Timeout = case ghtp_pl_queue:is_empty(State#ghtp_conn.pipeline_queue) of
		true  -> State#ghtp_conn.sock_timeout;
		false -> infinity
	end,
	NextState = receive
		% Requests
		{request, Request} ->
			handle_request(Request, State);

		% Complete response
		{response, Id, Response, KeepAlive} ->
			maybe_accept_request(
				handle_response(Id, Response, KeepAlive, State)
			);

		% Chunked response
		{chunked_response, Id, Response, KeepAlive} ->
			handle_chunked_resp(Id, Response, KeepAlive, State),
		{chunk, Id, Chunk} ->
			handle_chunk(Id, Chunk, State);
		{trailers, Id, Trailers, KeepAlive} ->
			maybe_accept_request(
				handle_trailers(Id, Chunk, KeepAlive, State)
			);

		% Partial response
		{partial_response, Id, Response, KeepAlive} ->
			handle_partial_resp(Id, Response, KeepAlive, State);
		{data, Id, Data} ->
			handle_data(Id, Data, State); 
		{end_of_data, Id} ->
			maybe_accept_request(handle_end_of_data(Id, State));

		% Reader exits
		{'EXIT', Reader, {bad_request, Reason}} ->
			handle_bad_request(Reason, State);
		{'EXIT', Reader, closed} -> % Reader got a socket closed
			close_and_exit(State);
		{'EXIT', Reader, Reason} -> % Reader died from reason X? 
			erlang:error(Reason);

		% Handler exits
		{'EXIT', Pid, Reason} -> % A request handler died
			handle_exit(Pid, Reason, State)
	after
		Timeout -> % Close persistant connection 
			close_and_exit(State)
	end,
	loop(NextState).

handle_request(Request, State) ->
	Id = ghtp_pl_queue:next_id(State#ghtp_conn.pipeline_queue),
	RequestArgs = [
		State#ghtp_conn.callback,
		State#ghtp_conn.callback_state,
		self(),
		State#ghtp_conn.socket,
		Id,
		Request,
	],
	Pid = spawn_link(ghtp_requesnt, execute, RequestArgs),
	NewQueue = ghtp_pl_queue:push(Pid, State#ghtp_conn.pipeline_queue),
	% Can we accept another request from the socket?
	QueueIsFull = ghtp_pl_queue:is_full(NewQueue),
	ReaderState = case {QueueIsFull, Request#request.method} of
		{false, "GET"} ->
			ghtp_reader:accept(State#ghtp_conn.reader),
			active;
		{false, "HEAD"} ->
			ghtp_reader:accept(State#ghtp_conn.reader),
			active;
		{_, _} ->
			idle
	end,
	% Here we could extract the keep-alive header and look for an indication
	% of how long the client want to keep alive and update our state,
	% but AFAIK we have no obligations to care about that. 
	State#ghtp_conn{pipeline_queue = NewQueue, reader_state = ReaderState}.

handle_response(Id, Response, KeepAlive, State) ->
	Queue = State#ghtp_conn.pipeline_queue,
	{Responses, DoKeepAlive, NewQueue} =
		ght_pl_queue:response(Id, Response, KeepAlive, Queue),
	if
		Responses =:= [] -> ok;
		Responses =/= [] -> gen_tcpd:send(State#ghtp_conn.socket, Responses)
	end,
	if
		DoKeepAlive -> State#ghtp_conn{pipeline_queue = NewQueue};
		not DoKeepAlive -> close_and_exit(State)
	end.

handle_exit(_Response, _Reason, State) ->
	State.

handle_bad_request(_Reason, State) ->
	State.

entity(#request{method = "POST", headers = Hdrs}, Socket) ->
	{entity_type(Hdrs), Socket};
entity(#request{method = "PUT", headers = Hdrs}, Socket) ->
	{entity_type(Hdrs), Socket};
entity(_, _) ->
	undefined.

entity_type(Hdrs) ->
	TransferEncoding = string:to_lower(
		ghtp_utils:header_value("transfer-encoding", Hdrs, "identity")
	),
	case TransferEncoding of
		"identity" -> identity;
		"chunked" -> chunked
	end.

maybe_accept_request(#ghtp_conn{reader_state = idle} = S) ->
	case ghtp_pl_queue:is_empty(S#ghtp_conn.pipeline_queue) of
		true ->
			ghtp_reader:accept(S#ghtp_conn.reader),
			S#ghtp_conn{reader_state = active};
		false ->
			ok
	end;
maybe_accept_request(State) ->
	State.

close_and_exit(#ghtp_conn{parent = Parent, reader = Reader, socket = Socket}) ->
	% Kill the reader since it's no longer needed.
	% (But we don't want to get the exit signal back.)
	unlink(Reader),
	exit(Reader, kill),
	gen_tcpd:close(Socket),
	% No need to send {'EXIT', self(), normal} to the parent
	unlink(Parent),
	exit(normal).
