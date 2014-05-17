-module(polyxena_tests).

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").


-define(short, 1/big-unsigned-unit:16).

encoding_test_() ->
    %% Socket = polyxena:init_client("192.168.60.15", 9160).
    %% io:format(" ~p:p ~n ~w", "~w", polyxena:cql_string(<< "String" >>)),
    [
     ?assertEqual(polyxena:cql_encode(string, << "String" >>),
                  [<<6:?short>>, << "String" >>]),
     ?assertEqual(polyxena:cql_encode(map, [{<< "Key1" >>, << "Value1" >>},
                                            {<< "Key2" >>, << "Value2" >>}]),
                  [<<2:?short>>,
                   [[<<4:?short>>, << "Key2" >>],
                    [<<6:?short>>, << "Value2" >>],
                    [<<4:?short>>, << "Key1" >>],
                    [<<6:?short>>, << "Value1" >>]]]),
     ?assertEqual(polyxena:cql_encode(string_list,
                                      [<< "Value1" >>, << "Value2" >>, << "Value3" >>]),
                  [<<3:?short>>,
                   [[<<6:?short>>, << "Value3" >>],
                    [<<6:?short>>, << "Value2" >>],
                    [<<6:?short>>, << "Value1" >>]]])
    ].

helper_test_() ->
    [?assertEqual(polyxena:has_flag(a, [c, b, a]), true),
     ?assertEqual(polyxena:has_flag(e, [c, b, a]), false),
     ?assertEqual(polyxena:has_flag(a, []), false)
    ].

%% TODO: FIGURE OUT HOW TO WRITE MORE THAN A SINGLE TEST
