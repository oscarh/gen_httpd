-module(http_client).

-export([
		send_request/4,
		receive_response/1,
		close_connections/0
	]).

send_request(URI, Method, Headers, Body) ->
	{_, Server, Port, Path} = parse_uri(URI),
	StatusLine = [Method, $\ , Path, $\ , "HTTP/1.1\r\n"],
	Request = [StatusLine, headers(Headers, iolist_size(Body)), Body],
	send(Server, Port, Request).

send(Server, Port, Request) ->
	case ets:info(connections) of
		undefined -> ets:new(connections, [named_table]);
		_         -> ok
	end,
	Socket = case ets:lookup(connections, {Server, Port}) of
		[{_, S}] ->
			S;
		[] ->
			Sockopts = [binary, {active, false}, binary],
			{ok, S} = gen_tcp:connect(Server, Port, Sockopts),
			ets:insert(connections, {{Server, Port}, S}),
			S
	end,
	case gen_tcp:send(Socket, Request) of
		{error, _} ->
			ets:delete(connections, {Server, Port}),
			send(Server, Port, Request);
		ok ->
			Socket
	end.

close_connections() ->
	case ets:info(connections) of
		undefined ->
			ok;
		_ ->
			ets:foldr(fun({_, Socket}, _) ->
						gen_tcp:close(Socket)
				end, nil, connections),
			ets:delete(connections)
	end.

headers(Headers, ContentLength) ->
	headers(Headers, ["\r\n"], ContentLength).

headers([{Name, Value} | T], Acc, ContentLength) ->
	headers(T, [Name, $:, $\ , Value, "\r\n" | Acc], ContentLength);
headers([], Acc, ContentLength) when ContentLength > 0 ->
	["Content-Length: ", integer_to_list(ContentLength), Acc, "\r\n"];
headers([], Acc, _) ->
	Acc.

receive_response(Socket) ->
	inet:setopts(Socket, [{packet, http}]),
	recv(Socket, nil, 0, [], 0, <<>>).

recv(Socket, Vsn0, Status0, Hdrs0, ContentLength, Body0) ->
	case gen_tcp:recv(Socket, 0, 10000) of
		{ok, {http_response, Vsn1, StatusCode, Description}} ->
			Status1 = {StatusCode, Description},
			recv(Socket, Vsn1, Status1, Hdrs0, ContentLength, Body0);
		{ok, {http_header, _, 'Content-Length', _, Length}} ->
			Hdrs1 = [{"Content-Length", Length} | Hdrs0],
			recv(Socket, Vsn0, Status0, Hdrs1, list_to_integer(Length), Body0);
		{ok, {http_header, _, Name, _, Value}} when is_atom(Name) ->
			Hdrs1 = [{atom_to_list(Name), Value} | Hdrs0],
			recv(Socket, Vsn0, Status0, Hdrs1, ContentLength, Body0);
		{ok, {http_header, _, Name, _, Value}} when is_list(Name) ->
			Hdrs1 = [{Name, Value} | Hdrs0],
			recv(Socket, Vsn0, Status0, Hdrs1, ContentLength, Body0);
		{ok, http_eoh} ->
			if
				ContentLength > 0 ->
					inet:setopts(Socket, [{packet, raw}]),
					case gen_tcp:recv(Socket, ContentLength, 10000) of
						{ok, Body1} ->
							{ok, {Vsn0, Status0, Hdrs0, Body1}};
						{error, Reason} ->
							exit(Reason)
					end;
				true ->
					{ok, {Vsn0, Status0, Hdrs0, Body0}}
			end;
		{error, closed} ->
			{error, closed}
	end.

parse_uri(URI) ->
	parse_uri(URI, [], [], [], []).

parse_uri([$:, $/, $/, S | T], Scheme, [], [], []) ->
	parse_uri(T, Scheme, [S], [], []);
parse_uri([S | T], Scheme, [], [], []) ->
	parse_uri(T, [S | Scheme], [], [], []);
parse_uri([$:, P | T], Scheme, Server, [], []) ->
	parse_uri(T, Scheme, Server, [P], []);
parse_uri([S | T], Scheme, Server, [], []) ->
	parse_uri(T, Scheme, [S | Server], [], []);
parse_uri([$/ | T], Scheme, Server, [], []) ->
	parse_uri(T, Scheme, Server, 80, [$/]);
parse_uri([$/ | T], Scheme, Server, Port, []) ->
	parse_uri(T, Scheme, Server, Port, [$/]);
parse_uri([P | T], Scheme, Server, Port, []) ->
	parse_uri(T, Scheme, Server, [P | Port], []);
parse_uri([P | T], Scheme, Server, Port, Path) ->
	parse_uri(T, Scheme, Server, Port, [P | Path]);
parse_uri([], Scheme, Server, [], Path) ->
	{lists:reverse(Scheme), lists:reverse(Server), 80, lists:reverse(Path)};
parse_uri([], Scheme, Server, Port0, Path) ->
	Port1 = list_to_integer(lists:reverse(Port0)),
	{lists:reverse(Scheme), lists:reverse(Server), Port1, lists:reverse(Path)}.
