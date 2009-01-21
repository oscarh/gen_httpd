-module(ghtp_reader).

-compile({inline, [maybe_atom_to_list/1]}).

-export([start/2, accept/1]).

-include("gen_httpd_int.hrl").

start(ConnHandler, Socket) ->
	loop(ConnHandler, Socket, #request{}).

loop(ConnHandler, Socket, State) ->
	NextState = case gen_tcp:recv(Socket, 0) of
		{ok, {http_request, Method, URI, Vsn}} ->
			State#request{
				method = maybe_atom_to_list(Method),
				uri = URI,
				vsn = Vsn
			};
		{ok, {http_header, _, Name, _, Value}} ->
			Hdr = {maybe_atom_to_list(Name), Value},
			State#request{headers = [Hdr | State#request.headers]};
		{ok, http_eoh} ->
			ConnHandler ! {request, State},
			receive '$accept' -> #request{} end;
		{error, {http_error, _}} ->
			exit(bad_request);
		{error, Reason} ->
			exit(Reason)
	end,
	loop(ConnHandler, Socket, NextState).

accept(Ref) ->
	Ref ! '$accept'.

maybe_atom_to_list(Atom) when is_atom(Atom) ->
	atom_to_list(Atom);
maybe_atom_to_list(List) when is_list(List) ->
	List.
