-module(ghtp_conn).
-export([init/5]).
-export([request_loop/1]).

-record(ghtp_conn, {
		parent,
		callback,
		callback_state,
		socket,
		sock_timeout
	}).

-include("gen_httpd_int.hrl").
-include("gen_httpd.hrl").

init(Parent, Callback, CallbackArg, Socket, SockTimeout) ->
	CallbackState = case Callback:init(Socket, CallbackArg) of
		{ok, S}        -> S;
		{stop, Reason} -> exit(Reason);
		Other          -> erlang:error({bad_return, Other})
	end,
	State = #ghtp_conn{
		parent = self(),
		callback = Callback,
		callback_state = CallbackState,
		socket = Socket,
		sock_timeout = SockTimeout
	},
	process_flag(trap_exit, true),
	spawn_link(?MODULE, request_loop, [State]),
	wait(Parent, Socket).

wait(Parent, Socket) ->
	% All we want to do here is wait for the socket to be closed for some
	% reason, or for a shutdown.
	receive
		{'EXIT', Parent, Reason} ->
			exit(Reason);
		{'EXIT', _, client_timeout} ->
			exit(normal);
		{'EXIT', _, client_closed} ->
			exit(normal);
		{'EXIT', Pid, Reason} ->
			handle_internal_error(Pid, Reason, Socket)
	end.

request_loop(#ghtp_conn{parent = Parent} = State) ->
	process_flag(trap_exit, true),
	link(Parent),
	NewCBState = execute_request(read_request(State), State),
	unlink(Parent),
	NewState = State#ghtp_conn{callback_state = NewCBState},
	% This is done to immediately free the memory used by the request
	spawn(?MODULE, request_loop, [NewState]).

read_request(#ghtp_conn{socket = Socket} = State) ->
	gen_tcpd:setopts(Socket, [{packet, http}]),
	Timeout = State#ghtp_conn.sock_timeout,
	TimerRef = erlang:send_after(Timeout, self(), timeout),
	Request = read_request_loop(State, TimerRef, Timeout, #request{}),
	cancel_timer(TimerRef),
	gen_tcpd:setopts(Socket, [{packet, raw}]),
	Request.

read_request_loop(State, _, Timeout, _) when Timeout < 1 ->
	terminate(client_timeout, State);
read_request_loop(State, TimerRef, Timeout, Request) ->
	Data = gen_tcpd:recv(State#ghtp_conn.socket, 0, Timeout),
	NextTimeout = case erlang:read_timer(TimerRef) of
		false -> 0;
		Time -> Time
	end,
	case Data of
		{ok, {http_request, Method, URI, Vsn}} ->
			UpdatedRequest = Request#request{
				method = maybe_atom_to_list(Method),
				uri = normalize_uri(URI),
				vsn = Vsn
			},
			read_request_loop(State, TimerRef, NextTimeout, UpdatedRequest);
		{ok, {http_header, _, Name, _, Value}} ->
			Hdr = {maybe_atom_to_list(Name), Value},
			UpdatedRequest = Request#request{
				headers = [Hdr | Request#request.headers]
			},
			read_request_loop(State, TimerRef, NextTimeout, UpdatedRequest);
		{ok, http_eoh} ->
			Request;
		{error, {http_error, HTTPReason} = Reason} ->
			handle_bad_request(Request, HTTPReason, State#ghtp_conn.socket),
			terminate(Reason, State);
		{error, timeout} ->
			terminate(client_timeout, State);
		{error, closed} ->
			terminate(client_closed, State);
		{error, Reason} ->
			terminate(Reason, State)
	end.

execute_request(Request, State) ->
	RequestArgs = [
		State#ghtp_conn.callback,
		State#ghtp_conn.callback_state,
		State#ghtp_conn.socket,
		Request
	],
	Pid = spawn_link(ghtp_request, execute, RequestArgs),
	receive
		{'EXIT', Pid, {done, false, NextCBState}} ->
			terminate(normal, State#ghtp_conn{callback_state = NextCBState});
		{'EXIT', Pid, {done, true, NextCBState}} ->
			NextCBState;
		{'EXIT', Pid, Reason} ->
			handle_internal_error(Pid, Reason, Request, State),
			terminate(Reason, State);
		{'EXIT', _, Reason} -> % Parent died, we should probably die :)
			terminate(Reason, State)
	end.

handle_bad_request(_Request, _Reason, _Socket) ->
	% TODO: reply here
	ok.

handle_internal_error(_Pid, _Reason, _Socket) ->
	% TODO: error report
	% TODO: reply here
	ok.

handle_internal_error(_Pid, _Reason, _Request, _State) ->
	% TODO: error report
	% TODO: reply here
	ok.

terminate(Reason, State) ->
	gen_tcpd:close(State#ghtp_conn.socket),
	(State#ghtp_conn.callback):terminate(Reason, State#ghtp_conn.callback_state),
	do_exit(Reason).

do_exit(client_closed) ->
	exit(normal);
do_exit(client_timeout) ->
	exit(normal);
do_exit({http_error, _}) ->
	exit(normal);
do_exit(Reason) ->
	exit(Reason).

maybe_atom_to_list(Atom) when is_atom(Atom) ->
	atom_to_list(Atom);
maybe_atom_to_list(List) when is_list(List) ->
	List.

normalize_uri({abs_path, Path}) ->
    Path;
normalize_uri({absoluteURI, http, Host, Port, Path}) ->
    lists:concat(["http://", Host, ":", Port, Path]);
normalize_uri({absoluteURI, https, Host, Port, Path}) ->
    lists:concat(["https://", Host, ":", Port, Path]);
normalize_uri({scheme, Scheme, Path}) ->
    lists:concat([Scheme, "://", Path]);
normalize_uri('*') ->
    "*";
normalize_uri(URI) when is_list(URI) ->
    URI.

cancel_timer(TimerRef) ->
	case erlang:cancel_timer(TimerRef) of
		false -> receive timeout -> ok end;
		_ -> ok
	end.
