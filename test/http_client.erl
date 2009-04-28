-module(http_client).

-export([
		format_request/4,
		receive_response/1,
		receive_response/2
	]).

format_request(Method, Path, Vsn, Headers) ->
    format_request(Method, Path, Vsn, Headers, []).

format_request(Method, Path, Vsn, Headers, Body) ->
	StatusLine = [Method, $\ , Path, $\ , Vsn , "\r\n"],
	[StatusLine, format_headers(Headers), Body].

format_headers(Headers) ->
	format_headers(Headers, ["\r\n"]).

format_headers([{Name, Value} | T], Acc) ->
	format_headers(T, [Name, $:, $\ , Value, "\r\n" | Acc]);
format_headers([], Acc) ->
	Acc.

receive_response(Socket) ->
    receive_response(Socket, gen_tcp).

receive_response(Socket, Mod) ->
	setopts(Socket, Mod, [{packet, http}]),
	recv(Socket, Mod, nil, 0, [], 0, <<>>).

setopts(Socket, gen_tcp, Options) ->
    inet:setopts(Socket, Options);
setopts(Socket, ssl, Options) ->
    ssl:setopts(Socket, Options).

recv(Socket, Mod, Vsn0, Status0, Hdrs0, ContentLength, Body0) ->
	case Mod:recv(Socket, 0) of
		{ok, {http_response, Vsn1, StatusCode, Description}} ->
			Status1 = {StatusCode, Description},
			recv(Socket, Mod, Vsn1, Status1, Hdrs0, ContentLength, Body0);
		{ok, {http_header, _, 'Content-Length', _, Length}} ->
			Hdrs1 = [{"Content-Length", Length} | Hdrs0],
            ILength = list_to_integer(Length),
			recv(Socket, Mod, Vsn0, Status0, Hdrs1, ILength, Body0);
		{ok, {http_header, _, Name, _, Value}} when is_atom(Name) ->
			Hdrs1 = [{atom_to_list(Name), Value} | Hdrs0],
			recv(Socket, Mod, Vsn0, Status0, Hdrs1, ContentLength, Body0);
		{ok, {http_header, _, Name, _, Value}} when is_list(Name) ->
			Hdrs1 = [{Name, Value} | Hdrs0],
			recv(Socket, Mod, Vsn0, Status0, Hdrs1, ContentLength, Body0);
		{ok, http_eoh} ->
			if
				ContentLength > 0 ->
                    setopts(Socket, Mod, [{packet, raw}]),
					case Mod:recv(Socket, ContentLength) of
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
