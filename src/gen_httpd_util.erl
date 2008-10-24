-module(gen_httpd_util).

-export([
		header_value/2,
		header_value/3,
		header_exists/2,
		update_header/3,
		remove_header/2
	]).
-export([parse_query/1, uri_encode/1, uri_decode/1]).
-export([status_line/2, format_headers/1]).
-export([internal_error_resp/1, bad_request_resp/1]).
-export([reason/1]).

-define(URI_ENCODE_ESCAPE,
	[
		% Reserved (RFC 2396: 2.2:
		$;, $/, $?, $:, $@, $&, $=, $+, $$, $,,
		% Excluded (RFC 2396: 2.4.3)
		0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
		20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 16#7f,
		$ , $<, $>, $#, $%, $", ${, $}, $|, $\\, $^, $[, $], $`
	]).

header_value(Name, Headers) ->
	header_value(Name, Headers, undefined).

header_value(Name, [{Name, Value} | _], _) ->
	Value;
header_value(Name, [{N, Value} | T], Default) ->
	case string:equal(Name, string:to_lower(N)) of
		true  -> Value;
		false -> header_value(Name, T, Default)
	end;
header_value(_, [], Default) ->
	Default.

header_exists(Name, [{Name, _} | _]) ->
	true;
header_exists(Name, [{N, _} | T]) ->
	case string:equal(Name, string:to_lower(N)) of
		true  -> true;
		false -> header_exists(Name, T)
	end;
header_exists(_, []) ->
	false.

update_header(Name, Value, Headers) ->
	update_header(Name, Value, Headers, []).

update_header(Name, Value, [{Name, _} | T], Acc) ->
	Acc ++ [{Name, Value}| T];
update_header(Name, Value, [{N, _} = Header | T], Acc) ->
	case string:equal(Name, string:to_lower(N)) of
		true -> 
			Acc ++ [{Name, Value}| T];
		false ->
			update_header(Name, Value, T, [Header | Acc])
	end;
update_header(Name, Value, [], Acc) ->
	[{Name, Value} | Acc].

remove_header(Name, Headers) ->
	remove_header(Name, Headers, []).

remove_header(Name, [{Name, _} | T], Acc) ->
	Acc ++ T; 
remove_header(Name, [{N, _} = Header | T], Acc) ->
	case string:equal(Name, string:to_lower(N)) of
		true -> 
			Acc ++ T;
		false ->
			remove_header(Name, T, [Header | Acc])
	end.

parse_query(QueryStr) ->
	case string:tokens(QueryStr, "&") of
		[] -> [];
		Args ->
			lists:map(fun(Arg) ->
						[Key, Value] = string:tokens(Arg, "="),
						{uri_decode(Key), uri_decode(Value)}
				end, Args)
	end.

uri_encode(Str) ->
	uri_encode(Str, []).

uri_encode([H | T], Acc) ->
	uri_encode(T, case lists:member(H, ?URI_ENCODE_ESCAPE) of
		true ->
			[A, B] = char_to_hex(H),
			[B, A, $% | Acc];
		false ->
			[H | Acc]
	end);
uri_encode([], Acc) ->
	lists:reverse(Acc).

uri_decode(Str) ->
	uri_decode(Str, []).

uri_decode([$%, A, B | Rest], Acc) ->
	uri_decode(Rest, [erlang:list_to_integer([A, B], 16) | Acc]);
uri_decode([H | Rest], Acc) ->
	uri_decode(Rest, [H | Acc]);
uri_decode([], Acc) ->
	lists:reverse(Acc).

internal_error_resp(Version) ->
	Headers = [{"connection", "close"}],
	[status_line(Version, 500), format_headers(Headers)].

bad_request_resp(true) ->
	bad_request_resp([{"connection", "close"}]);
bad_request_resp(false) ->
	bad_request_resp([]);
bad_request_resp(Headers) ->
	[status_line("HTTP/1.1", 400), format_headers(Headers)].

reason(200) -> "OK";
reason(400) -> "Bad Request";
reason(500) -> "Internal server error".

format_headers(Headers) ->
	[lists:map(fun({Name, Value}) ->
					[Name, $:, $\ , Value, $\r, $\n]
			end, Headers), $\r, $\n].

status_line(Vsn, {Status, Reason}) ->
	[Vsn, $\ , integer_to_list(Status), $\ , Reason, $\r, $\n];
status_line(Vsn, Status) ->
	status_line(Vsn, {Status, reason(Status)}).

char_to_hex(Char) ->
	string:right(erlang:integer_to_list(Char, 16), 2, $0).
