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
%%% @copyright 2008 Martin Carlson, Oscar Hellström
%%% @author Martin Carlson <martin@martinc.eu>
%%% @author Oscar Hellström <oscar@hellstrom.st> [http://oscar.hellstrom.st]
%%% @version {@version}, {@date}, {@time}
%%% @doc
%%% Utility module for gen_httpd itself, but also for applications using
%%% gen_httpd.
%%% @end
%%% ----------------------------------------------------------------------------
-module(ghtp_utils).

-export([
		header_value/2,
		header_value/3,
		header_values/2,
		header_exists/2,
		update_header/3,
		remove_header/2
	]).
-export([timeout/2]).
-export([parse_query/1, uri_encode/1, uri_decode/1]).
-export([
	format_response/3,
	format_response/4,
	status_line/2,
	format_headers/1
	]).
-export([chunk_size/1]).
-export([internal_error_resp/0, bad_request_resp/0]).
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

header_values(Name, Headers) ->
	header_values(Name, Headers, []).

header_values(Name, [{Field, Value} | Headers], Acc) ->
	case string:to_lower(Field) of
		Name -> header_values(Name, Headers, [Value | Acc]);
		_    -> header_values(Name, Headers, Acc)
	end;
header_values(_, [], Acc) ->
	Acc.

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

%%% @private
internal_error_resp() ->
	[status_line({1, 1}, 500), format_headers([{"connection", "close"}])].

%%% @private
bad_request_resp() ->
	[status_line({1, 1}, 400), format_headers([{"connection", "close"}])].

%%% @private
reason(100) -> "Continue";
reason(101) -> "Switching Protocols";

reason(200) -> "OK";
reason(201) -> "Created";
reason(202) -> "Accepted";
reason(203) -> "Non-Authoritative Information";
reason(204) -> "No Content";
reason(206) -> "Partial Content";

reason(300) -> "Multiple Choices";
reason(301) -> "Moved Permanently";
reason(302) -> "Found";
reason(303) -> "See Other";
reason(304) -> "Not Modified";
reason(305) -> "Use Proxy";
reason(307) -> "Temporary Redirect";

reason(400) -> "Bad Request";
reason(401) -> "Unauthorized";
reason(402) -> "Payment Required";
reason(403) -> "Forbidden";
reason(404) -> "Not Found";
reason(405) -> "Method Not Allowed";
reason(406) -> "Not Acceptable";
reason(407) -> "Proxy Authentication Required";
reason(408) -> "Request Time-Out";
reason(409) -> "Conflict";
reason(410) -> "Gone";
reason(411) -> "Length Required";
reason(412) -> "Precondition Failed";
reason(413) -> "Request Entity Too Large";
reason(414) -> "Request-URI Too Large";
reason(415) -> "Unsupported Media Type";
reason(416) -> "Requested range tot satisfiable";
reason(417) -> "Expectation Failed";

reason(500) -> "Internal server error";
reason(501) -> "Not Implemented";
reason(502) -> "Bad Gateway";
reason(503) -> "Service Unavailable";
reason(504) -> "Gateway Time-Out";
reason(505) -> "HTTP Version not supported".

%% @private
format_response(Vsn, Status, Hdrs) ->
	format_response(Vsn, Status, Hdrs, []).

%% @private
format_response(Vsn, Status, Hdrs, Body) ->
	[status_line(Vsn, Status), format_headers(Hdrs), Body].


%% @private
format_headers(Headers) ->
	[lists:map(fun({Name, Value}) ->
					[Name, $:, $\ , Value, $\r, $\n]
			end, Headers), $\r, $\n].

%% @private
status_line({Major, Minor}, {StatusCode, Reason}) when is_integer(StatusCode) ->
	[
		"HTTP/", integer_to_list(Major), ".", integer_to_list(Minor), $\ ,
		integer_to_list(StatusCode), $\ , Reason, $\r, $\n
	];
status_line(Vsn, StatusCode) when is_integer(StatusCode) ->
	status_line(Vsn, {StatusCode, reason(StatusCode)}).

%% @private
timeout(infinity, _) ->
	infinity;
timeout(MS, Start) ->
	MS - (timer:now_diff(now(), Start) div 1000).

%% @private
chunk_size(Bin) ->
	erlang:list_to_integer(lists:reverse(chunk_size(Bin, [])), 16).

chunk_size(<<$;, _/binary>>, Acc) ->
	Acc;
chunk_size(<<"\r\n", _/binary>>, Acc) ->
	Acc;
chunk_size(<<Char, Rest/binary>>, Acc) ->
	chunk_size(Rest, [Char | Acc]).

char_to_hex(Char) ->
	string:right(erlang:integer_to_list(Char, 16), 2, $0).
