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
-module(ghtp_request).

-export([execute/4]).

-import(ghtp_utils, [
		header_exists/2,
		header_value/2,
		header_value/3,
		status_line/2,
		format_headers/1
	]).

-include("gen_httpd_int.hrl").

-define(TYPE(REPLY), element(1, REPLY)).
-define(STATUS(REPLY), element(2, REPLY)).
-define(HDRS(REPLY), element(3, REPLY)).
-define(BODY(REPLY), element(4, REPLY)).
-define(CBSTATE(REPLY), element(size(REPLY), REPLY)).

execute(CB, CBState, Socket, Request) ->
	Method = Request#request.method,
	Vsn = Request#request.vsn,
	URI = Request#request.uri,
	ReqHdrs = Request#request.headers,
	case expect_continue(ReqHdrs) of
		true ->
			handle_continue(Socket, Method, URI, Vsn, ReqHdrs, CB, CBState);
		false ->
			hadle_request(Socket, Method, URI, Vsn, ReqHdrs, CB, CBState)
	end.

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

handle_continue(Socket, Method, URI, Vsn, ReqHdrs, CB, CBState) ->
	case CB:handle_continue(Method, URI, Vsn, ReqHdrs, CBState) of
		{continue, RespHdrs, NextCBState} ->
			send_status_and_hdr(Socket, Vsn, 100, RespHdrs),
			hadle_request(Socket, Method, URI, Vsn, ReqHdrs, CB, NextCBState);
		{reply, Status, ReplyHdrs, Body, NextCBState} -> 
			KeepAlive = 
				handle_reply(Socket, Vsn, ReqHdrs, Status, ReplyHdrs, Body),
			{KeepAlive, NextCBState};
		Other ->
			erlang:error({bad_return, Other})
	end.

handle_reply(Socket, Vsn, ReqHdrs, Status, ReplyHdrs, Body) ->
	HdrKeepAlive = keep_alive(Vsn, ReqHdrs, ReplyHdrs),
	case handle_body(ReplyHdrs, Body) of
		{chunked, Hdrs, Chunk, ReaderFun} ->
			send_status_and_hdr(Socket, Vsn, Status, Hdrs),
			send_chunk(Socket, Chunk),
			case send_chunks(Socket, ReaderFun) of
				false -> false;
				true  -> HdrKeepAlive
			end;
		{chunked, Hdrs, ReaderFun} ->
			send_status_and_hdr(Socket, Vsn, Status, Hdrs),
			case send_chunks(Socket, ReaderFun) of
				false -> false;
				true  -> HdrKeepAlive
			end;
		{partial, Hdrs, Part, ReaderFun} ->
			send_status_and_hdr(Socket, Vsn, Status, Hdrs),
			gen_tcpd:send(Socket, Part),
			send_parts(Socket, ReaderFun),
			HdrKeepAlive;
		{complete, Hdrs, Body} ->
			Data = format_response(Vsn, Status, Hdrs, Body),
			gen_tcpd:send(Socket, Data),
			HdrKeepAlive
	end.

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
			gen_tcpd:send(Socket, ["0\r\n", ghtp_utils:format_headers(T)]),
			% We've already done some protocol checks for this since we
			% checked when the status and headers was sent. We're just
			% checking again so that we're not getting any Connection: close
			% trailers.
			KeepAlive = case ghtp_utils:header_value("connection", T) of
				"close" -> false;
				_       -> true
			end,
			KeepAlive;
		Other ->
			erlang:error({bad_return, Other})
	end.

send_chunk(Socket, Chunk) ->
	ChunkSize = iolist_size(Chunk),
	Data = [erlang:integer_to_list(ChunkSize, 16), "\r\n", Chunk],
	gen_tcpd:send(Socket, Data).

send_status_and_hdr(Socket, Vsn, Status, Hdrs) ->
	gen_tcpd:send(Socket, format_response(Vsn, Status, Hdrs)).

format_response(Vsn, Status, Hdrs) ->
	StatusLine = status_line(Vsn, Status),
	FormatedHdrs = format_headers(Hdrs),
	[StatusLine, FormatedHdrs].

format_response(Vsn, Status, Hdrs, Body) ->
	[format_response(Vsn, Status, Hdrs), Body].

entity("POST", Hdrs, Socket) ->
    Type = entity_type(Hdrs),
	{element(1, Type), entity_reader(Type, Socket)};
entity("PUT", Hdrs, Socket) ->
    Type = entity_type(Hdrs),
	{element(1, Type), entity_reader(Type, Socket)};
entity(_, _, Socket) ->
    {undefined, Socket}.

entity_type(Hdrs) ->
    case header_value("content-length", Hdrs) of
        undefined -> 
            case header_value("transfer-encoding", Hdrs) of
                undefined -> {undefined, nil};
                "chunked" -> {chunked, nil}
            end;
        Length ->
            {identity, list_to_integer(Length)}
    end.

entity_reader({identity, Length}, Socket) ->
    fun(Timeout) -> gen_tcpd:recv(Socket, Length, Timeout) end;
entity_reader({chunked, _}, Socket) ->
    fun(Timeout) -> read_chunk(Socket, Timeout) end;
entity_reader({undefined, _}, Socket) ->
    Socket.

read_chunk(Socket, Timeout) ->
    Start = now(),
    gen_tcpd:setopts(Socket, [{packet, line}]),
    case gen_tcpd:recv(Socket, 0, Timeout) of
        {ok, Line} ->
            String = binary_to_list(Line),
            [AsciiChunkSize | _] = string:tokens(String, " \r\n"),
            ChunkSize = erlang:list_to_integer(AsciiChunkSize, 16),
            TimeElapsed = timer:now_diff(now(), Start) div 1000,
            NewTimeout = Timeout - TimeElapsed,
            if
                ChunkSize > 0 ->
                    gen_tcpd:setopts(Socket, [{packet, raw}]),
                    case gen_tcpd:recv(Socket, ChunkSize + 2, NewTimeout) of
                        {ok, <<Data:ChunkSize/binary, $\r, $\n>>} ->
                            {chunk, Data};
                        Other ->
                            Other
                    end;
                ChunkSize =:= 0 ->
					read_trailers(Socket, [], NewTimeout)
            end;
        Other ->
            Other
    end.

read_trailers(Socket, Acc, Timeout) ->
	Start = now(),
	case gen_tcpd:recv(Socket, 0, Timeout) of
		{ok, <<"\r\n">>} ->
			{trailers, Acc};
		{ok, Bin} ->
			Hdr = ghtp_utils:parse_header(Bin),
            TimeElapsed = timer:now_diff(now(), Start) div 1000,
            NewTimeout = Timeout - TimeElapsed,
			read_trailers(Socket, [Hdr | Acc], NewTimeout)
	end.

handle_body(Hdrs, {partial, Reader}) ->
	Type = case header_exists("content-length", Hdrs) of
		true  -> partial;
		false -> chunked
	end,
	UpdatedHdrs = case header_exists("transfer-encoding", Hdrs) of
		true  -> Hdrs;
		false -> [{"Transfer-Encoding", "chunked"} | Hdrs]
	end,
	{Type, UpdatedHdrs, Reader};
handle_body(Hdrs, {partial, Part, Reader}) ->
	{Type, UpdatedHdrs, _} = handle_body(Hdrs, {partial, Reader}),
	{Type, UpdatedHdrs, Part, Reader};
handle_body(Hdrs, Body) when is_list(Body); is_binary(Body) ->
	UpdatedHdrs = case header_exists("content-length", Hdrs) of
		false ->
			Length = iolist_size(Body),
			[{"Content-Length", integer_to_list(Length)} | Hdrs];
		true  ->
			Hdrs
	end,
	{complete, UpdatedHdrs, Body};
handle_body(_, Body) ->
	erlang:error({bad_return, {body, Body}}).

expect_continue(Headers) ->
	case header_value("expect", Headers, undefined) of
		"100-continue" -> true;
		undefined      -> false;
		_              -> false
	end.

keep_alive(Vsn, ReqHdrs, RespHdrs) ->
	% First of all, let the callback module decide
	case header_value("connection", RespHdrs) of
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
	case string:to_lower(header_value("connection", Hdrs, "")) of
		"keep-alive" -> true;
		_            -> false
	end.

post_1_0_keep_alive(Hdrs) ->
	case ghtp_utils:header_value("connection", Hdrs) of
		"close" -> false;
		_       -> true
	end.
