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
%%%
%%% == References ==
%%% RFC2396: http://www.ietf.org/rfc/rfc2396.txt
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
-export([split_uri/1, parse_query/1, uri_encode/1, uri_decode/1]).
-export([
	format_response/3,
	format_response/4,
	status_line/2,
	format_headers/1
	]).

-include("gen_httpd_types.hrl").

-define(URI_ENCODE_ESCAPE,
	[
		% Reserved (RFC 2396: 2.2:
		$;, $/, $?, $:, $@, $&, $=, $+, $$, $,,
		% Excluded (RFC 2396: 2.4.3)
		0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
		20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 16#7f,
		$ , $<, $>, $#, $%, $", ${, $}, $|, $\\, $^, $[, $], $`
	]).

%% @type header() = {string(), string()}.

%% @spec (Name, Headers) -> Values
%% Name = string()
%% Headers = [header()]
%% Values = [string()]
%% @doc
%% Returns all values for the header named `Name'.
%% `Name' is expected to be a lowercase string.
%% Returns a list of all values found in the list of headers. Useful for for
%% instance "Transfer-Encoding", of which several can be applied.
%% @end
-spec header_values(string(), [header()]) -> [header()].
header_values(Name, Headers) ->
	header_values(Name, Headers, []).

%% @spec (Name, Headers) -> Value | undefined
%%   Name = string()
%%   Headers = [header()]
%% @doc
%% Returns the value of the header named `Name'.
%% `Name' is expected to be a lowercase string.
%% Returns `undefined' if the header isn't in the list of headers.
%% @end
-spec header_value(string(), [header()]) -> string() | undefined.
header_value(Name, Headers) ->
	header_value(Name, Headers, undefined).

%% @spec (Name, Headers, Default :: Default) -> Value | Default
%%   Name = string()
%%   Headers = [header()]
%% @doc
%% Returns the value of the header named `Name'.
%% `Name' is expected to be a lowercase string.
%% Returns `Default' if the header isn't in the list of headers.
%% @end
-spec header_value(string(), [header()], Type) -> string() | Type.
header_value(Name, [{N, Value} | T], Default) ->
	case string:equal(Name, string:to_lower(N)) of
		true  -> Value;
		false -> header_value(Name, T, Default)
	end;
header_value(_, [], Default) ->
	Default.

%% @spec (Name, Headers) -> boolean()
%%   Name = string()
%%   Headers = [header()]
%% @doc
%% Checks if a header is defined in the list of headers. `Name' is expected
%% to be a lowercase string.
%% @end
-spec header_exists(string(), [header()]) -> true | false.
header_exists(Name, [{N, _} | T]) ->
	case string:equal(Name, string:to_lower(N)) of
		true  -> true;
		false -> header_exists(Name, T)
	end;
header_exists(_, []) ->
	false.

%% @spec (Name, Value, Headers) -> Headers
%%   Name = string()
%%   Value = string()
%%   Headers = [header()]
%% @doc
%% Replaces the current value for the first occurrence of `Name' with
%% `Value'. If `Name' isn't define, it's added to the list of headers.
-spec update_header(string(), string(), [header()]) -> [header()].
update_header(Name, Value, Headers) ->
	update_header(Name, Value, Headers, []).

%% @spec (Name, Headers) -> Headers
%%   Name = string()
%%   Headers = [header()]
%% @doc
%% Removes the first occurrence of `Name' from the list of headers.
%% @end
-spec remove_header(string(), [header()]) -> [header()].
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

%% @spec (String) -> {URI, QueryString}
%%   String = string()
%%   URI = string()
%%   QueryString = string()
%% @doc
%% Splits a URI in to the two parts, the URI and the query string part.
%% If there isn't any query string, the original string is returned... after
%% some gymnastics.
%% 
%% @end
-spec split_uri(string()) -> {string(), string()}.
split_uri(String) ->
	split_uri(String, []).

%% @spec (QueryString :: QueryString) -> [{Key, Value}]
%%   QueryString = string()
%%   Key = string()
%%   Value = string()
%% @doc Turns a query string (the part after the first `?' in a URI to
%% a list of key value tuples.
%% The query string can be `Path'/`URI' with {@link split_uri/1}.
%% @end
-spec parse_query(string()) -> [{string(), string()}].
parse_query(QueryStr) ->
	case string:tokens(QueryStr, "&") of
		[] -> [];
		Args ->
			lists:map(fun(Arg) ->
						case string:tokens(Arg, "=") of
							[Key, Value] ->
								{uri_decode(Key), uri_decode(Value)};
							[Key] ->
								{uri_decode(Key), ""}
						end
				end, Args)
	end.

%% @spec uri_encode(String) -> URIEncodedString
%%   String = string()
%%   URIEncodedString = string()
%% @doc
%% Encodes a string according to RFC2396.
%% All reserver or excluded characters are turned in to a "%" HEX HEX
%% notation.
%% @end
-spec uri_encode(string()) -> string().
uri_encode(Str) ->
	uri_encode(Str, []).

%% @spec uri_decode(URIEncodedString) -> String
%%   URIEncodedString = string()
%%   String = string()
%% @doc
%% Decodes a string according to RFC2396.
%% All "%" HEX HEX sequences are turned in to the character represented by
%% the HEX HEX number.
%% @end
-spec uri_decode(string()) -> string().
uri_decode(Str) ->
	uri_decode(Str, []).

uri_decode([$%, A, B | Rest], Acc) ->
	uri_decode(Rest, [erlang:list_to_integer([A, B], 16) | Acc]);
uri_decode([H | Rest], Acc) ->
	uri_decode(Rest, [H | Acc]);
uri_decode([], Acc) ->
	lists:reverse(Acc).

%% @private
-spec format_response(version(), status(), [header()]) -> iolist().
format_response(Vsn, Status, Hdrs) ->
	format_response(Vsn, Status, Hdrs, []).

%% @private
-spec format_response(version(), status(), [header()], iolist() | binary()) ->
    iolist().
format_response(Vsn, Status, Hdrs, Body) ->
	[status_line(Vsn, Status), format_headers(Hdrs), Body].


%% @private
-spec format_headers({string(), string()}) -> iolist().
format_headers(Headers) ->
	[lists:map(fun({Name, Value}) ->
					[Name, $:, $\ , Value, $\r, $\n]
			end, Headers), $\r, $\n].

%% @private
-spec status_line(version(), status()) ->
	iolist().
status_line({Major, Minor}, {StatusCode, Reason}) when is_integer(StatusCode) ->
	[
		"HTTP/", integer_to_list(Major), ".", integer_to_list(Minor), $\ ,
		integer_to_list(StatusCode), $\ , Reason, $\r, $\n
	];
status_line(Vsn, StatusCode) when is_integer(StatusCode) ->
	status_line(Vsn, {StatusCode, reason(StatusCode)}).

%% @private
-spec timeout(timeout(), {integer(), integer(), integer()}) ->
	timeout().
timeout(infinity, _) ->
	infinity;
timeout(MS, Start) ->
	MS - (timer:now_diff(now(), Start) div 1000).

%%% Internal functions

header_values(Name, [{Field, Value} | Headers], Acc) ->
	case string:to_lower(Field) of
		Name -> header_values(Name, Headers, [Value | Acc]);
		_    -> header_values(Name, Headers, Acc)
	end;
header_values(_, [], Acc) ->
	Acc.

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

split_uri([$? | QueryString], Acc) ->
	{lists:reverse(Acc), QueryString};
split_uri([Char | Tail], Acc) ->
	split_uri(Tail, [Char | Acc]);
split_uri([], Acc) ->
	{lists:reverse(Acc), ""}.

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

char_to_hex(Char) ->
	string:right(erlang:integer_to_list(Char, 16), 2, $0).

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
