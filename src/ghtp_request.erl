-module(ghtp_request).
-export([execute/6]).

execute(CB, CBState, Parent, Socket, RequestId, Request) ->
	Method = Request#request.method,
	VSN = Request#request.vsn,
	URI = Request#request.uri,
	Headers = Request#request.headers,
	Entity = entity(Request, Socket),
	Result = CB:handle_request(Method, URI, Vsn, Headers, Entity, CBState),
	handle_result(Parent, RequestId, Vsn, Headers, Result),
	% No need to send {'EXIT', self(), normal} to the parent
	unlink(Parent).

handle_result(Parent, Id, Vsn, ReqHdrs, {reply, Status, Hdrs, Body}) ->
	UpdatedHdrs = case ghtp_utils:header_exists("content-length", Hdrs) of
		false -> [{"Content-Length", iolist_size(Body)} | Hdrs];
		true -> Hdrs
	end,
	Response = format_response(Vsn, Status, UpdatedHdrs, Body),
	KeepAlive = keep_alive(Vsn, ReqHdrs, UpdatedHdrs),
	Parent ! {response, Id, Response, KeepAlive};
handle_result(Parent, Id, Vsn, ReqHdrs,
			{chunked_reply, Status, Hdrs, Reader}) ->
	UpdatedHdrs = case ghtp_utils:header_exists("transfer-encoding", Hdrs) of
		true -> Hdrs;
		false -> [{"Transfer-Encoding", "chunked"} | Hdrs]
	end,
	Response = format_response(Vsn, Status, UpdatedHdrs),
	KeepAlive = keep_alive(Vsn, ReqHdrs, UpdatedHdrs),
	Parent ! {chunked_response, Id, Response, KeepAlive},
	read_chunks(Parent, Id, Reader);
handle_result(Parent, Id, Vsn, ReqHdrs,
			{partial_reply, Status, Hdrs, Body, Reader}) ->
	HasContentLength = ghtp_utils:header_exists("content-length", Hdrs),
	HasConnectionClose = case ghtp_utils:header_value("connection", Hdrs) of
		undefined -> false;
		"close" -> true
	end,
	UpdatedHdrs = case {HasContentLength, HasConnectionClose} of
		{true, _} -> Hdrs;
		{false, true} -> Hdrs;
		{false, false} -> [{"Connection", "close"} | Hdrs];
		{_, _} -> Hdrs
	end,
	Response = format_response(Vsn, Status, UpdatedHdrs, Body),
	KeepAlive = keep_alive(Vsn, ReqHdrs, UpdatedHdrs),
	Parent ! {partial_response, Id, Response, KeepAlive},
	read_data(Parent, Id, Reader).

read_chunks(Parent, Id, Reader) ->
	case Reader() of
		{chunk, C} ->
			ChunkSize = iolist_size(C),
			Chunk = [erlang:integer_to_list(ChunkSize, 16), "\r\n", C],
			Parent ! {chunk, Id, Chunk},
			read_chunks(Parent, Reader);
		{trailers, T} ->
			% We've already done some protocol checks for this since we
			% checked when the status and headers was sent. We're just
			% checking again so that we're not getting any Connection: close
			% trailers.
			KeepAlive = case ghtp_utils:header_value("connection", T) of
				"close" -> false;
				_       -> true
			end,
			Parent ! {trailers, Id, ghtp_utils:format_headers(T), KeepAlive};
		Other ->
			erlang:error({bad_return, Other})
	end.

read_data(Parent, Id, Reader) ->
	case Reader() of
		{data, D} ->
			Parent ! {data, Id, D};
		end_of_data ->
			Parent ! {end_of_data, Id};
		Other ->
			erlang:error({bad_return, Other})
	end.

format_response(Vsn, Status, Hdrs) ->
	StatusLine = ghtp_utils:status_line(Vsn, Status),
	FormatedHdrs = ghtp_utils:format_headers(UpdatedHdrs),
	[StatusLine, FormatedHdrs].

format_response(Vsn, Status, Hdrs) ->
	StatusLine = ghtp_utils:status_line(Vsn, Status),
	FormatedHdrs = ghtp_utils:format_headers(UpdatedHdrs),
	[StatusLine, FormatedHdrs, Body].

keep_alive(Vsn, ReqHdrs, RespHdrs) ->
	% First of all, let the callback module decide
	case ghtp_utils:header_value("connection", RespHdrs) of
		"close" -> 
			false;
		_ -> % no preference (that we understand)
			case Vsn of
				{0,_} -> pre_1_1_keep_alive(ReqHdrs);
				{1,0} -> pre_1_1_keep_alive(ReqHdrs)
				{1,_} -> post_1_1_keep_alive(ReqHdrs)
			end
	end.

pre_1_1_keep_alive(Hdrs) ->
	case string:to_lower(ghtp_utils:header_value("connection", Hdrs)) of
		"keep-alive" -> true;
		_            -> false
	end.

post_1_1_keep_alive(Hdrs) ->
	case ghtp_utils:header_value("connection", Hdrs) of
		"close" -> false;
		_       -> true
	end.
