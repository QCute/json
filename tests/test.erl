-module(test).
-export([test/0]).
-include_lib("stdlib/include/assert.hrl").

test() ->
    Data = #{
        <<"a">> => <<"b">>,
        <<"c">> => 1,
        <<"d">> => [2,3,4],
        <<"e">> => #{
            <<"g">> => <<"h">>
        },
        <<"i">> => false,
        <<"j">> => [null],
        <<"k">> => 1.23
    },
    String = json:encode(Data),
    ?assert(Data == json:decode(String)).
