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
%%% @private
-module(ghtp_request).

-export([read_body/3]).
-export([execute/4]).

-include("gen_httpd_int.hrl").
-include("gen_httpd_types.hrl").

-include_lib("gen_tcpd/include/gen_tcpd_types.hrl").

-define(MIN(A, B), (if A =< B -> A; B =< A -> B end)).

-record(chunked, {remaining_bytes, socket}).
-record(identity, {remaining_bytes, socket}).

%% Read the uploaded body

-spec read_body(pos_integer() | complete, timeout(), #chunked{} | #identity{}) ->
	{ok, {binary(), #identity{}}} | {ok, {binary(), done}} |
	{chunk, {binary(), #chunked{}}} | {trailers, [header()]}.
read_body(Length, Timeout, #chunked{} = State) ->
	RemainingBytes = State#chunked.remaining_bytes,
	read_chunk(Length, Timeout, RemainingBytes, State#chunked.socket, []);
read_body(Length, Timeout, State) ->
	RemainingBytes = State#identity.remaining_bytes,
	read_identity(Length, Timeout, RemainingBytes, State#identity.socket).

%%% Execute a request

-spec execute(atom(), CBState, gen_tcpd_socket(), #request{}) ->
	{true | false, CBState}.
execute(CB, CBState, Socket, Request) ->
	Method = Request#request.method,
	Vsn = Request#request.vsn,
	URI = Request#request.uri,
	ReqHdrs = Request#request.headers,
	case expect_continue(ReqHdrs) of
		true  -> handle_continue(Socket, Method, URI, Vsn, ReqHdrs, CB, CBState);
		false -> hadle_request(Socket, Method, URI, Vsn, ReqHdrs, CB, CBState)
	end.

%%% Handle requests

hadle_request(Socket, Method, URI, Vsn, ReqHdrs, CB, CBState) ->
	Entity = entity(Method, ReqHdrs, Socket),
	case CB:handle_request(Method, URI, Vsn, ReqHdrs, Entity, CBState) of
		{reply, Status, ReplyHdrs, Body, NextCBState} ->
			KeepAlive =
				handle_reply(Socket, Vsn, ReqHdrs, Status, ReplyHdrs, Body),
			{KeepAlive, NextCBState};
		Other ->
			erlang:error({bad_return, Other})
	end.

%%% Handle uploaded entities

entity("GET", Hdrs, _) ->
	ensure_no_entity(Hdrs);
entity("HEAD", Hdrs, _) ->
	ensure_no_entity(Hdrs);
entity("TRACE", Hdrs, _) ->
	ensure_no_entity(Hdrs);
entity(_, Hdrs, Socket) ->
	case entity_info(Hdrs) of
		undefined ->
			undefined;
		chunked ->
			State = #chunked{remaining_bytes = 0, socket = Socket},
			{chunked, State};
		{identity, Length} ->
			State = #identity{remaining_bytes = Length, socket = Socket},
			{identity, State}
	end.

ensure_no_entity(Hdrs) ->
	ContentLength = ghtp_utils:header_exists("content-length", Hdrs),
	TransferEncoding = ghtp_utils:header_exists("transfer-encoding", Hdrs),
	case {ContentLength, TransferEncoding} of
		{false, false} -> ok;
		_              -> throw(bad_request)
	end.

entity_info(Hdrs) ->
	ContentLength = ghtp_utils:header_value("content-length", Hdrs),
	case {ContentLength, is_chunked(Hdrs)} of
		{_, true} ->
			% XXX is it an error to specify chunked encoding *and* a
			% content length? Well as long as it's chunked, I guess it's
			% chunked :)
			chunked;
		{undefined, false} ->
			undefined;
		{ContentLength, false}  ->
			Length = try list_to_integer(ContentLength)
				catch error:badarg -> throw(bad_request)
			end,
			{identity, Length}
	end.

is_chunked(Hdrs) ->
	case ghtp_utils:header_values("transfer-encoding", Hdrs) of
		[] ->
			false;
		[TransferEncoding] ->
			string:equal(string:to_lower(TransferEncoding), "chunked");
		[_, _ | _] ->
			throw(not_implemented)
	end.

%%% Handle Expect: 100-continue headers

handle_continue(Socket, Method, URI, Vsn, ReqHdrs, CB, CBState) ->
	case CB:handle_continue(Method, URI, Vsn, ReqHdrs, CBState) of
		{continue, RespHdrs, NextCBState} ->
			send_response(Socket, Vsn, 100, RespHdrs),
			hadle_request(Socket, Method, URI, Vsn, ReqHdrs, CB, NextCBState);
		{reply, Status, ReplyHdrs, Body, NextCBState} -> 
			KeepAlive = 
				handle_reply(Socket, Vsn, ReqHdrs, Status, ReplyHdrs, Body),
			{KeepAlive, NextCBState};
		Other ->
			erlang:error({bad_return, Other})
	end.

%%% Handle responses

handle_reply(Socket, Vsn, ReqHdrs, Status, ReplyHdrs, Body) ->
	HdrKeepAlive = keep_alive(Vsn, ReqHdrs, ReplyHdrs),
	case handle_body(ReplyHdrs, Body) of
		{chunked, Hdrs, Chunk, ReaderFun} ->
			send_response(Socket, Vsn, Status, Hdrs),
			send_chunk(Socket, Chunk),
			case send_chunks(Socket, ReaderFun) of
				false -> false;
				true  -> HdrKeepAlive
			end;
		{chunked, Hdrs, ReaderFun} ->
			send_response(Socket, Vsn, Status, Hdrs),
			case send_chunks(Socket, ReaderFun) of
				false -> false;
				true  -> HdrKeepAlive
			end;
		{partial, Hdrs, Part, ReaderFun} ->
			send_response(Socket, Vsn, Status, Hdrs),
			gen_tcpd:send(Socket, Part),
			send_parts(Socket, ReaderFun),
			HdrKeepAlive;
		{partial, Hdrs, ReaderFun} ->
			send_response(Socket, Vsn, Status, Hdrs),
			send_parts(Socket, ReaderFun),
			HdrKeepAlive;
		{complete, Hdrs, Body} ->
			Data = ghtp_utils:format_response(Vsn, Status, Hdrs, Body),
			gen_tcpd:send(Socket, Data),
			HdrKeepAlive
	end.

handle_body(Hdrs, {partial, Reader}) ->
	Type = case ghtp_utils:header_exists("content-length", Hdrs) of
		true  -> partial;
		false -> chunked
	end,
	UpdatedHdrs = case ghtp_utils:header_exists("transfer-encoding", Hdrs) of
		true  -> Hdrs;
		false -> [{"Transfer-Encoding", "chunked"} | Hdrs]
	end,
	{Type, UpdatedHdrs, Reader};
handle_body(Hdrs, {partial, Part, Reader}) ->
	{Type, UpdatedHdrs, _} = handle_body(Hdrs, {partial, Reader}),
	{Type, UpdatedHdrs, Part, Reader};
handle_body(Hdrs, Body) when is_list(Body); is_binary(Body) ->
	UpdatedHdrs = case ghtp_utils:header_exists("content-length", Hdrs) of
		false ->
			Length = iolist_size(Body),
			[{"Content-Length", integer_to_list(Length)} | Hdrs];
		true  ->
			Hdrs
	end,
	{complete, UpdatedHdrs, Body};
handle_body(_, Body) ->
	erlang:error({bad_return, {body, Body}}).

send_parts(Socket, Reader) ->
	case Reader() of
		{data, D} ->
			gen_tcpd:send(Socket, D),
			send_parts(Socket, Reader);
		end_of_data ->
			ok;
		Other ->
			erlang:error({bad_return, Other})
	end.

send_chunks(Socket, Reader) ->
	case Reader() of
		{chunk, C} ->
			send_chunk(Socket, C),
			send_chunks(Socket, Reader);
		{trailers, T} ->
			% We've already done some protocol checks for this since we
			% checked when the status and headers was sent. We're just
			% checking again so that we're not getting any Connection: close
			% trailers.
			KeepAlive = case ghtp_utils:header_value("connection", T) of
				"close" -> false;
				_       -> true
			end,
			gen_tcpd:send(Socket, ["0\r\n", ghtp_utils:format_headers(T)]),
			KeepAlive;
		Other ->
			erlang:error({bad_return, Other})
	end.

send_chunk(Socket, Chunk) ->
	ChunkSize = iolist_size(Chunk),
	Data = [erlang:integer_to_list(ChunkSize, 16), "\r\n", Chunk, "\r\n"],
	gen_tcpd:send(Socket, Data).

expect_continue(Headers) ->
	ExpectToken = ghtp_utils:header_value("expect", Headers, "undefined"),
	case string:to_lower(ExpectToken) of
		"100-continue" -> true;
		_              -> false
	end.

send_response(Socket, Vsn, Status, Hdrs) ->
	gen_tcpd:send(Socket, ghtp_utils:format_response(Vsn, Status, Hdrs)).

%%% Connection related helper functions

keep_alive(Vsn, ReqHdrs, RespHdrs) ->
	% First of all, let the callback module decide
	case ghtp_utils:header_value("connection", RespHdrs) of
		"close" -> 
			false;
		_ -> % no preference (that we understand)
			case Vsn of
				{0,_} -> pre_1_1_keep_alive(ReqHdrs);
				{1,0} -> pre_1_1_keep_alive(ReqHdrs);
				{1,_} -> post_1_0_keep_alive(ReqHdrs)
			end
	end.

pre_1_1_keep_alive(Hdrs) ->
	ConnectionToken = ghtp_utils:header_value("connection", Hdrs, ""),
	case string:to_lower(ConnectionToken) of
		"keep-alive" -> true;
		_            -> false
	end.

post_1_0_keep_alive(Hdrs) ->
	case ghtp_utils:header_value("connection", Hdrs) of
		"close" -> false;
		_       -> true
	end.

%%% Help functions for reading of bodies 

read_identity(_, Timeout, _, _) when Timeout < 0 -> % infinity > 0
	{error, timeout};
read_identity(Length, Timeout, RemainingBytes, Socket) ->
	case gen_tcpd:recv(Socket, ?MIN(Length, RemainingBytes), Timeout) of
		{ok, Data} ->
			NewRemainingBytes = RemainingBytes - size(Data),
			State = case NewRemainingBytes of
				0 ->
					http_eob;
				Bytes ->
					#identity{
						remaining_bytes = Bytes,
						socket = Socket
					}
			end,
			{ok, {Data, State}};
		Other ->
			Other
	end.

read_chunk(0, _, RemainingBytes, Socket, Acc) ->
	State = #chunked{remaining_bytes = RemainingBytes, socket = Socket},
	{chunk, {list_to_binary(Acc), State}};
read_chunk(_, Timeout, _, _, _) when Timeout < 0 -> % infinity > 0
	{error, timeout};
read_chunk(_, Timeout, trailers, Socket, _) ->
	read_trailers(Timeout, Socket, []);
read_chunk(Length, Timeout, 0, Socket, Acc) ->
	Start = now(),
	ok = gen_tcpd:setopts(Socket, [{packet, line}]),
	case gen_tcpd:recv(Socket, 0, Timeout) of
		{ok, ChunkSizeExt} ->
			case chunk_size(ChunkSizeExt) of
				0 ->
					State = #chunked{
						remaining_bytes = trailers,
						socket = Socket
					},
					{chunk, {list_to_binary(Acc), State}};
				ChunkSize ->
					ok = gen_tcpd:setopts(Socket, [{packet, raw}]),
					NewTimeout = ghtp_utils:timeout(Timeout, Start),
					read_chunk(Length, NewTimeout, ChunkSize, Socket, Acc)
			end;
		Other ->
			Other
	end;
read_chunk(Length, Timeout, RemainingBytes, Socket, Acc) ->
	Start = now(),
	{DataSize, BytesToRead} = if
		Length >= RemainingBytes ->
			{RemainingBytes, RemainingBytes + 2};
		Length =< RemainingBytes ->
			{length(Length), Length}
	end,
	case gen_tcpd:recv(Socket, BytesToRead, Timeout) of
		{ok, <<Data:DataSize/binary, _/binary>>} ->
			NewTimeout = ghtp_utils:timeout(Timeout, Start),
			NewLength = if
				Length =:= complete ->
					complete;
				is_integer(Length) ->
					Length - DataSize
			end,
			Bytes = RemainingBytes - DataSize,
			NewAcc = [Acc, Data],
			read_chunk(NewLength, NewTimeout, Bytes, Socket, NewAcc);
		Other ->
			Other
	end.

chunk_size(Bin) ->
	erlang:list_to_integer(lists:reverse(chunk_size(Bin, [])), 16).

chunk_size(<<$;, _/binary>>, Acc) ->
	Acc;
chunk_size(<<"\r\n", _/binary>>, Acc) ->
	Acc;
chunk_size(<<Char, Rest/binary>>, Acc) ->
	chunk_size(Rest, [Char | Acc]).

read_trailers(Timeout, _, _) when Timeout =/= infinity, Timeout < 0 ->
	{error, timeout};
read_trailers(Timeout, Socket, Trailers) ->
	Start = now(),
	ok = gen_tcpd:setopts(Socket, [{packet, httph}]),
	case gen_tcpd:recv(Socket, 0, Timeout) of
		{ok, http_eoh} ->
			{trailers, Trailers};
		{ok, {http_header, _, Name, _, Value}} when is_atom(Name) ->
			Trailer = {atom_to_list(Name), Value},
			NewTimeout = ghtp_utils:timeout(Timeout, Start),
			read_trailers(NewTimeout, Socket, [Trailer | Trailers]);
		{ok, {http_header, _, Name, _, Value}} when is_list(Name) ->
			Trailer = {Name, Value},
			NewTimeout = ghtp_utils:timeout(Timeout, Start),
			read_trailers(NewTimeout, Socket, [Trailer | Trailers]);
		Other ->
			Other
	end.
