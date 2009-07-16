%%% @private
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

-spec init(pid(), module(), _, _, timeout()) -> _.
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
			ok;
		{'EXIT', _, client_closed} ->
			ok;
		{'EXIT', _, normal} ->
			ok;
		{'EXIT', Pid, Reason} ->
			handle_internal_error(Pid, Reason, Socket)
	end.

-spec request_loop(#ghtp_conn{}) -> _.
request_loop(#ghtp_conn{parent = Parent} = State) ->
	% Race condition, parent can have been shut down
	try link(Parent)
	catch error:noproc -> exit(normal)
	end,
	NewCBState = execute_request(read_request(State), State),
	unlink(Parent),
	NewState = State#ghtp_conn{callback_state = NewCBState},
	% This is done to immediately free the memory used by the request
	spawn(?MODULE, request_loop, [NewState]).

read_request(#ghtp_conn{socket = Socket} = State) ->
	gen_tcpd:setopts(Socket, [{packet, http}]),
	Timeout = State#ghtp_conn.sock_timeout,
	Request = read_request_loop(State, Timeout, #request{}),
	gen_tcpd:setopts(Socket, [{packet, raw}]),
	Request.

read_request_loop(State, Timeout, _) when Timeout < 0 ->
	terminate(client_timeout, State);
read_request_loop(State, Timeout, Request) ->
	Start = now(),
	Data = gen_tcpd:recv(State#ghtp_conn.socket, 0, Timeout),
	case Data of
		{ok, {http_request, Method, URI, Vsn}} ->
			UpdatedRequest = Request#request{
				method = maybe_atom_to_list(Method),
				uri = normalize_uri(URI),
				vsn = Vsn
			},
			NewTimeout = ghtp_utils:timeout(Timeout, Start),
			read_request_loop(State, NewTimeout, UpdatedRequest);
		{ok, {http_header, _, Name, _, Value}} ->
			Hdr = {maybe_atom_to_list(Name), Value},
			UpdatedRequest = Request#request{
				headers = [Hdr | Request#request.headers]
			},
			NewTimeout = ghtp_utils:timeout(Timeout, Start),
			read_request_loop(State, NewTimeout, UpdatedRequest);
		{ok, http_eoh} ->
			Request;
		{error, {http_error, _} = Reason} ->
			handle_bad_request(State#ghtp_conn.socket),
			terminate(Reason, State);
		{error, timeout} ->
			terminate(client_timeout, State);
		{error, closed} ->
			terminate(client_closed, State);
		{error, Reason} ->
			terminate(Reason, State)
	end.

execute_request(Request, State) ->
	Callback = State#ghtp_conn.callback,
	CallbackState = State#ghtp_conn.callback_state,
	Socket = State#ghtp_conn.socket,
	Version = Request#request.vsn,
	{KeepAlive, NextCBState} = try 
		ghtp_request:execute(Callback, CallbackState, Socket, Request)
		catch
			not_imlemented ->
				Response = ghtp_utils:format_response(Version, 501, []),
				gen_tcpd:send(Socket, Response),
				terminate(normal, State);
			length_required ->
				Response = ghtp_utils:format_response(Version, 411, []),
				gen_tcpd:send(Socket, Response),
				terminate(normal, State);
			bad_request ->
				Response = ghtp_utils:format_response(Version, 400, []),
				gen_tcpd:send(Socket, Response),
				terminate(normal, State)
	end,
	if
		KeepAlive ->
			NextCBState;
		not KeepAlive ->
			terminate(normal, State#ghtp_conn{callback_state = NextCBState})
	end.

handle_bad_request(Socket) ->
	gen_tcpd:send(Socket, ghtp_utils:format_response({1, 1}, 400, [])),
	ok.

handle_internal_error(_Pid, Reason, Socket) ->
	gen_tcpd:send(Socket, ghtp_utils:format_response({1, 1}, 500, [])),
	exit(Reason).

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
