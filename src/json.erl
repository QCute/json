%%%-------------------------------------------------------------------
%%% @doc
%%% json object encoder/decoder
%%% @end
%%%-------------------------------------------------------------------
-module(json).
%% API
-export([encode/1, decode/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc encode json
-spec encode(Data :: term()) -> binary().
encode(Data) ->
    encode_value([terminate], <<>>, Data).

%% @doc decode json
-spec decode(Binary :: binary()) -> undefined | number() | boolean() | map() | list() | binary().
decode(Binary) when is_binary(Binary) ->
    decode_value(Binary, Binary, 0, [terminate]).

%%%===================================================================
%%% Encode Part
%%%===================================================================

%% value
encode_value(Stack, Acc, <<Value/binary>>) ->
    encode_string(Stack, <<Acc/binary, $">>, Value);
encode_value(Stack, Acc, Value) when is_integer(Value) ->
    Binary = erlang:integer_to_binary(Value),
    encode_continue(Stack, <<Acc/binary, Binary/binary>>);
encode_value(Stack, Acc, Value) when is_float(Value) ->
    Binary = erlang:list_to_binary(io_lib_format:fwrite_g(Value)),
    encode_continue(Stack, <<Acc/binary, Binary/binary>>);
encode_value(Stack, Acc, Value) when is_list(Value) ->
    encode_list(Stack, <<Acc/binary, $[>>, Value);
encode_value(Stack, Acc, Value) when is_map(Value) ->
    encode_map(Stack, <<Acc/binary, ${>>, maps:to_list(Value));
encode_value(Stack, Acc, undefined) ->
    encode_continue(Stack, <<Acc/binary, "null">>);
encode_value(Stack, Acc, true) ->
    encode_continue(Stack, <<Acc/binary, "true">>);
encode_value(Stack, Acc, false) ->
    encode_continue(Stack, <<Acc/binary, "false">>);
encode_value(Stack, Acc, Value) when is_atom(Value) ->
    encode_string(Stack, <<Acc/binary, $">>, erlang:atom_to_binary(Value)).

%% list
encode_list(Stack, Acc, []) ->
    encode_continue(Stack, <<Acc/binary, $]>>);
encode_list(Stack, Acc, [Head | Tail]) ->
    encode_value([list, Tail | Stack], Acc, Head).

%% map
encode_map(Stack, Acc, []) ->
    encode_continue(Stack, <<Acc/binary, $}>>);
encode_map(Stack, Acc, [{Key, Value} | Tail]) ->
    encode_map_key([key, Value, map, Tail | Stack], Acc, Key).

%% key
encode_map_key(Stack, Acc, <<Key/binary>>) ->
    encode_string(Stack, <<Acc/binary, $">>, Key);
encode_map_key(Stack, Acc, Key) ->
    encode_string(Stack, <<Acc/binary, $">>, erlang:atom_to_binary(Key)).

%% TODO UTF-8 convert
%% string
encode_string(Stack, Acc, <<>>) ->
    encode_continue(Stack, <<Acc/binary, $">>);
encode_string(Stack, Acc, <<$\b, Rest/binary>>) ->
    encode_string(Stack, <<Acc/binary, $\\, $b>>, Rest);
encode_string(Stack, Acc, <<$\t, Rest/binary>>) ->
    encode_string(Stack, <<Acc/binary, $\\, $t>>, Rest);
encode_string(Stack, Acc, <<$\n, Rest/binary>>) ->
    encode_string(Stack, <<Acc/binary, $\\, $n>>, Rest);
encode_string(Stack, Acc, <<$\f, Rest/binary>>) ->
    encode_string(Stack, <<Acc/binary, $\\, $f>>, Rest);
encode_string(Stack, Acc, <<$\r, Rest/binary>>) ->
    encode_string(Stack, <<Acc/binary, $\\, $r>>, Rest);
encode_string(Stack, Acc, <<$", Rest/binary>>) ->
    encode_string(Stack, <<Acc/binary, $\\, $">>, Rest);
encode_string(Stack, Acc, <<$\\, Rest/binary>>) ->
    encode_string(Stack, <<Acc/binary, $\\, $\\>>, Rest);
encode_string(Stack, Acc, <<C:8, Rest/binary>>) ->
    encode_string(Stack, <<Acc/binary, C:8>>, Rest).

%% continue
encode_continue([terminate], Acc) ->
    Acc;
encode_continue([list, List = [_ | _] | Stack], Acc) ->
    encode_list(Stack, <<Acc/binary, $,>>, List);
encode_continue([list, List | Stack], Acc) ->
    encode_list(Stack, Acc, List);
encode_continue([key, Value | Stack], Acc) ->
    encode_value(Stack, <<Acc/binary, $:>>, Value);
encode_continue([map, Map = [_ | _ ] | Stack], Acc) ->
    encode_map(Stack, <<Acc/binary, $,>>, Map);
encode_continue([map, Map | Stack], Acc) ->
    encode_map(Stack, Acc, Map).

%%%===================================================================
%%% Decode Part
%%%===================================================================

%% whitespace
decode_value(<<$\t, Rest/binary>>, Original, Skip, Stack) ->
    decode_value(Rest, Original, Skip + 1, Stack);
decode_value(<<$\n, Rest/binary>>, Original, Skip, Stack) ->
    decode_value(Rest, Original, Skip + 1, Stack);
decode_value(<<$\r, Rest/binary>>, Original, Skip, Stack) ->
    decode_value(Rest, Original, Skip + 1, Stack);
decode_value(<<$\s, Rest/binary>>, Original, Skip, Stack) ->
    decode_value(Rest, Original, Skip + 1, Stack);
%% number minus
decode_value(<<$-, Rest/binary>>, Original, Skip, Stack) ->
    decode_number_minus(Rest, Original, Skip, Stack, 1);
%% zero or float number
decode_value(<<$0, Rest/binary>>, Original, Skip, Stack) ->
    decode_number_zero(Rest, Original, Skip, Stack, 1);
%% integer
decode_value(<<$1, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 1);
decode_value(<<$2, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 2);
decode_value(<<$3, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 3);
decode_value(<<$4, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 4);
decode_value(<<$5, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 5);
decode_value(<<$6, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 6);
decode_value(<<$7, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 7);
decode_value(<<$8, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 8);
decode_value(<<$9, Rest/binary>>, Original, Skip, Stack) ->
    decode_number(Rest, Original, Skip, Stack, 1, 1, 9);
%% string
decode_value(<<$", Rest/binary>>, Original, Skip, Stack) ->
    decode_string(Rest, Original, Skip + 1, Stack, 0);
%% array
decode_value(<<$[, Rest/binary>>, Original, Skip, Stack) ->
    decode_value(Rest, Original, Skip + 1, [array, [] | Stack]);
decode_value(<<$], Rest/binary>>, Original, Skip, [array, [] | Stack]) ->
    decode_continue(Rest, Original, Skip + 1, Stack, []);
%% object
decode_value(<<${, Rest/binary>>, Original, Skip, Stack) ->
    decode_key(Rest, Original, Skip + 1, [[] | Stack]);
%% boolean
decode_value(<<"true", Rest/binary>>, Original, Skip, Stack) ->
    decode_continue(Rest, Original, Skip + 4, Stack, true);
decode_value(<<"false", Rest/binary>>, Original, Skip, Stack) ->
    decode_continue(Rest, Original, Skip + 5, Stack, false);
%% null
decode_value(<<"null", Rest/binary>>, Original, Skip, Stack) ->
    decode_continue(Rest, Original, Skip + 4, Stack, undefined).

%% number minus
decode_number_minus(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_zero(Rest, Original, Skip, Stack, Length + 1);
decode_number_minus(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 1);
decode_number_minus(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 2);
decode_number_minus(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 3);
decode_number_minus(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 4);
decode_number_minus(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 5);
decode_number_minus(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 6);
decode_number_minus(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 7);
decode_number_minus(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 8);
decode_number_minus(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, -1, 9).

%% float
decode_number_zero(<<$., Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction(Rest, Original, Skip, Stack, Length + 1);
%% float with exp
decode_number_zero(<<$e, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_copy(Rest, Original, Skip + Length + 1, Stack, <<"0">>);
decode_number_zero(<<$E, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_copy(Rest, Original, Skip + Length + 1, Stack, <<"0">>);
%% continue
decode_number_zero(<<Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_continue(Rest, Original, Skip + Length, Stack, 0).

%% number
decode_number(<<$0, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 0);
decode_number(<<$1, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 1);
decode_number(<<$2, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 2);
decode_number(<<$3, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 3);
decode_number(<<$4, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 4);
decode_number(<<$5, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 5);
decode_number(<<$6, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 6);
decode_number(<<$7, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 7);
decode_number(<<$8, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 8);
decode_number(<<$9, Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_number(Rest, Original, Skip, Stack, Length + 1, Sign, Base * 10 + 9);
%% number with exp
decode_number(<<$., Rest/binary>>, Original, Skip, Stack, Length, _, _) ->
    decode_number_fraction(Rest, Original, Skip, Stack, Length + 1);
decode_number(<<$e, Rest/binary>>, Original, Skip, Stack, Length, _, _) ->
    <<_:Skip/binary, Prefix:Length/binary, _/binary>> = Original,
    decode_number_exp_copy(Rest, Original, Skip + Length + 1, Stack, Prefix);
decode_number(<<$E, Rest/binary>>, Original, Skip, Stack, Length, _, _) ->
    <<_:Skip/binary, Prefix:Length/binary, _/binary>> = Original,
    decode_number_exp_copy(Rest, Original, Skip + Length + 1, Stack, Prefix);
%% continue
decode_number(<<Rest/binary>>, Original, Skip, Stack, Length, Sign, Base) ->
    decode_continue(Rest, Original, Skip + Length, Stack, Sign * Base).

%% number fraction
decode_number_fraction(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1).

decode_number_fraction_continue(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
%% number fraction with exp
decode_number_fraction_continue(<<$e, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp(Rest, Original, Skip, Stack, Length + 1);
decode_number_fraction_continue(<<$E, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp(Rest, Original, Skip, Stack, Length + 1);
%% continue
decode_number_fraction_continue(<<Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, Part:Length/binary, _/binary>> = Original,
    decode_continue(Rest, Original, Skip + Length, Stack, erlang:binary_to_float(Part)).

%% number exp
decode_number_exp(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
%% number exp with sign
decode_number_exp(<<$+, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_sign(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp(<<$-, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_sign(Rest, Original, Skip, Stack, Length + 1).

%% number exp sign
decode_number_exp_sign(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_sign(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1).

%% number exp continue
decode_number_exp_continue(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
decode_number_exp_continue(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
%% continue
decode_number_exp_continue(<<Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, Part:Length/binary, _/binary>> = Original,
    decode_continue(Rest, Original, Skip + Length, Stack, erlang:binary_to_float(Part)).

%% number exp copy
decode_number_exp_copy(<<$0, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$1, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$2, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$3, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$4, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$5, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$6, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$7, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$8, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$9, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
%% number exp copy with sign
decode_number_exp_copy(<<$+, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_sign(Rest, Original, Skip, Stack, 1, Prefix);
decode_number_exp_copy(<<$-, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    decode_number_exp_copy_sign(Rest, Original, Skip, Stack, 1, Prefix).

decode_number_exp_copy_sign(<<$0, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$1, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$2, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$3, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$4, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$5, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$6, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$7, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$8, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_sign(<<$9, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix).

decode_number_exp_copy_continue(<<$0, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$1, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$2, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$3, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$4, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$5, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$6, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$7, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$8, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
decode_number_exp_copy_continue(<<$9, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    decode_number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
%% continue
decode_number_exp_copy_continue(<<Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    <<_:Skip/binary, Part:Length/binary, _/binary>> = Original,
    String = <<Prefix/binary, ".0e", Part/binary>>,
    decode_continue(Rest, Original, Skip + Length, Stack, erlang:binary_to_float(String)).

%% array
decode_array(<<$\t, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_array(Rest, Original, Skip + 1, Stack, Value);
decode_array(<<$\n, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_array(Rest, Original, Skip + 1, Stack, Value);
decode_array(<<$\r, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_array(Rest, Original, Skip + 1, Stack, Value);
decode_array(<<$\s, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_array(Rest, Original, Skip + 1, Stack, Value);
decode_array(<<$,, Rest/binary>>, Original, Skip, [Acc | Stack], Value) ->
    decode_value(Rest, Original, Skip + 1, [array, [Value | Acc] | Stack]);
decode_array(<<$], Rest/binary>>, Original, Skip, [Acc | Stack], Value) ->
    decode_continue(Rest, Original, Skip + 1, Stack, lists:reverse(Acc, [Value])).

%% object
decode_object(<<$\t, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_object(Rest, Original, Skip + 1, Stack, Value);
decode_object(<<$\n, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_object(Rest, Original, Skip + 1, Stack, Value);
decode_object(<<$\r, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_object(Rest, Original, Skip + 1, Stack, Value);
decode_object(<<$\s, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_object(Rest, Original, Skip + 1, Stack, Value);
decode_object(<<$,, Rest/binary>>, Original, Skip, [Key, Acc | Stack], Value) ->
    decode_key(Rest, Original, Skip + 1, [[{Key, Value} | Acc] | Stack]);
decode_object(<<$}, Rest/binary>>, Original, Skip, [Key, Acc | Stack], Value) ->
    decode_continue(Rest, Original, Skip + 1, Stack, maps:from_list([{Key, Value} | Acc])).

%% key
decode_key(<<$\t, Rest/binary>>, Original, Skip, Stack) ->
    decode_key(Rest, Original, Skip + 1, Stack);
decode_key(<<$\n, Rest/binary>>, Original, Skip, Stack) ->
    decode_key(Rest, Original, Skip + 1, Stack);
decode_key(<<$\r, Rest/binary>>, Original, Skip, Stack) ->
    decode_key(Rest, Original, Skip + 1, Stack);
decode_key(<<$\s, Rest/binary>>, Original, Skip, Stack) ->
    decode_key(Rest, Original, Skip + 1, Stack);
decode_key(<<$", Rest/binary>>, Original, Skip, Stack) ->
    decode_string(Rest, Original, Skip + 1, [key | Stack], 0);
decode_key(<<$}, Rest/binary>>, Original, Skip, [[] | Stack]) ->
    decode_continue(Rest, Original, Skip + 1, Stack, maps:new()).

%% key - value
decode_key(<<$\t, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_key(Rest, Original, Skip + 1, Stack, Value);
decode_key(<<$\n, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_key(Rest, Original, Skip + 1, Stack, Value);
decode_key(<<$\r, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_key(Rest, Original, Skip + 1, Stack, Value);
decode_key(<<$\s, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_key(Rest, Original, Skip + 1, Stack, Value);
decode_key(<<$:, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_value(Rest, Original, Skip + 1, [object, Value | Stack]).

%% string
%% TODO UTF-8 check
decode_string(<<$", Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, String:Length/binary, _/binary>> = Original,
    decode_continue(Rest, Original, Skip + Length + 1, Stack, String);
decode_string(<<$\\, Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, String:Length/binary, _/binary>> = Original,
    decode_escape(Rest, Original, Skip + Length, Stack, String);
decode_string(<<_, Rest/binary>>, Original, Skip, Stack, Length) ->
    decode_string(Rest, Original, Skip, Stack, Length + 1).

%% TODO UTF-8 check
decode_string_acc(<<$", Rest/binary>>, Original, Skip, Stack, Length, Acc) ->
    <<_:Skip/binary, String:Length/binary, _/binary>> = Original,
    decode_continue(Rest, Original, Skip + Length + 1, Stack, <<Acc/binary, String/binary>>);
decode_string_acc(<<$\\, Rest/binary>>, Original, Skip, Stack, Length, Acc) ->
    <<_:Skip/binary, String:Length/binary, _/binary>> = Original,
    decode_escape(Rest, Original, Skip + Length, Stack, <<Acc/binary, String/binary>>);
decode_string_acc(<<_, Rest/binary>>, Original, Skip, Stack, Length, Acc) ->
    decode_string_acc(Rest, Original, Skip, Stack, Length + 1, Acc).

%% escape string
decode_escape(<<$b, Rest/binary>>, Original, Skip, Stack, Acc) ->
    decode_string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\b>>);
decode_escape(<<$t, Rest/binary>>, Original, Skip, Stack, Acc) ->
    decode_string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\t>>);
decode_escape(<<$n, Rest/binary>>, Original, Skip, Stack, Acc) ->
    decode_string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\n>>);
decode_escape(<<$f, Rest/binary>>, Original, Skip, Stack, Acc) ->
    decode_string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\f>>);
decode_escape(<<$r, Rest/binary>>, Original, Skip, Stack, Acc) ->
    decode_string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\r>>);
decode_escape(<<$", Rest/binary>>, Original, Skip, Stack, Acc) ->
    decode_string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $">>);
decode_escape(<<$/, Rest/binary>>, Original, Skip, Stack, Acc) ->
    decode_string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $/>>);
decode_escape(<<$\\, Rest/binary>>, Original, Skip, Stack, Acc) ->
    decode_string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\\>>);
decode_escape(<<$u, Escape:4/binary, Rest/binary>>, Original, Skip, Stack, Acc) ->
    %% the escape unicode
    decode_unicode_high(Rest, Original, Skip + 2 + 4, Stack, binary_to_integer(Escape, 16), Acc).

%% high part of unicode
decode_unicode_high(<<$\\, $u, Escape:4/binary, Rest/binary>>, Original, Skip, Stack, High, Acc) when 16#D800 =< High andalso High =< 16#DBFF ->
    %% the surrogate pair
    decode_unicode_low(Rest, Original, Skip + 2 + 4, Stack, High, binary_to_integer(Escape, 16), Acc);
decode_unicode_high(<<Rest/binary>>, Original, Skip, Stack, Unicode, Acc) when 0 < Unicode andalso Unicode < 16#DC00 orelse 16#DFFF < Unicode ->
    %% not the second part of surrogate pair (without first part)
    decode_string_acc(Rest, Original, Skip, Stack, 0, <<Acc/binary, Unicode/utf8>>).

%% low part of unicode
decode_unicode_low(Rest, Original, Skip, Stack, High, Low, Acc) when 16#DC00 =< Low andalso Low =< 16#DFFF ->
    <<Unicode/utf16>> = <<High:16, Low:16>>,
    decode_string_acc(Rest, Original, Skip, Stack, 0, <<Acc/binary, Unicode/utf8>>).

%% continue
decode_continue(<<Rest/binary>>, Original, Skip, [terminate | Stack], Value) ->
    decode_terminate(Rest, Original, Skip, Stack, Value);
decode_continue(<<Rest/binary>>, Original, Skip, [array | Stack], Value) ->
    decode_array(Rest, Original, Skip, Stack, Value);
decode_continue(<<Rest/binary>>, Original, Skip, [key | Stack], Value) ->
    decode_key(Rest, Original, Skip, Stack, Value);
decode_continue(<<Rest/binary>>, Original, Skip, [object | Stack], Value) ->
    decode_object(Rest, Original, Skip, Stack, Value).

%% terminate
decode_terminate(<<$\t, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_terminate(Rest, Original, Skip + 1, Stack, Value);
decode_terminate(<<$\n, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_terminate(Rest, Original, Skip + 1, Stack, Value);
decode_terminate(<<$\r, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_terminate(Rest, Original, Skip + 1, Stack, Value);
decode_terminate(<<$\s, Rest/binary>>, Original, Skip, Stack, Value) ->
    decode_terminate(Rest, Original, Skip + 1, Stack, Value);
decode_terminate(<<_/binary>>, _Original, _Skip, _Stack, Value) ->
    Value.
