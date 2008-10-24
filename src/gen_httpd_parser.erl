%%% ----------------------------------------------------------------------------
%%% Copyright 2008
%%% Martin Carlson, martin@erlang-consulting.com
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
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @author Oscar Hellström <oscar@hellstrom.st> [http://oscar.hellstrom.st]
%%% @version {@version}, {@date}, {@time}
%%% @doc
%%% 
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd_parser).

-export([parse/1]).

parse(Request) ->
	{Method, URL, Vsn, Rest0} = parse_request(Request),
	{Headers, Rest1} = parse_headers(Rest0, []),
	{{Method, URL, Vsn, Headers}, Rest1}.

parse_request("\r\n" ++ Rest) ->
	parse_request(Rest);
parse_request(Req) ->
	{Meth, Rest0} = parse_method(Req, []),
	{Path, Rest1} = parse_path(Rest0, []),
	{VSN, Rest2} = parse_vsn(Rest1, []),
	{Meth, Path, VSN, Rest2}.

parse_headers("\r\n" ++ Rest, Headers) ->
	{Headers, Rest};
parse_headers(Req, Headers) ->
	{Key, Rest0} = parse_header_key(Req, []),
	{Value, Rest1} = parse_header_value(Rest0, []),
	parse_headers(Rest1, [{Key, Value}|Headers]).

parse_method(" " ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_method([H|T], Acc) ->
	parse_method(T, [H|Acc]);
parse_method([], _) ->
	exit(incomplete).

parse_path(" " ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_path([H|T], Acc) ->
	parse_path(T, [H|Acc]);
parse_path([], _) ->
	exit(incomplete).

parse_vsn("\r\n" ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_vsn([H|T], Acc) ->
	parse_vsn(T, [H|Acc]);
parse_vsn([], _) ->
	exit(incomplete).

parse_header_key(":" ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_header_key([H|T], Acc) ->
	parse_header_key(T, [H|Acc]);
parse_header_key([], _) ->
	exit(incomplete).

parse_header_value("\r\n" ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_header_value([H|T], Acc) ->
	parse_header_value(T, [H|Acc]);
parse_header_value([], _) ->
	exit(incomplete).

