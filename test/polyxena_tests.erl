-module(polyxena_tests).

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").


-define(short, 1/big-unsigned-unit:16).

encoding_test_() ->
    %% Socket = polyxena:init_client("192.168.60.15", 9160).
    %% io:format(" ~p:p ~n ~w", "~w", polyxena:cql_string(<< "String" >>)),
    [

    ].

helper_test_() ->
    [?assertEqual(polyxena_connection:has_flag(a, [c, b, a]), true),
     ?assertEqual(polyxena_connection:has_flag(e, [c, b, a]), false),
     ?assertEqual(polyxena_connection:has_flag(a, []), false)
    ].

%% TODO: FIGURE OUT HOW TO WRITE MORE THAN A SINGLE TEST
