-module(polyxena_tests).

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").


-define(short, 1/big-unsigned-unit:16).

mymodule_test_() ->
    %% Socket = polyxena:init_client("192.168.60.15", 9160).
    %% io:format(" ~p:p ~n ~w", "~w", polyxena:cql_string(<< "String" >>)),
    [
     ?assertEqual(polyxena:cql_string(<< "String" >>), [<<6:?short>>, << "String" >>]),
     ?assertEqual(polyxena:cql_map([{<< "Key1" >>, << "Value1" >>},
                                    {<< "Key2" >>, << "Value2" >>}]),
                  [<<2:?short>>,
                   [[<<4:?short>>, << "Key2" >>],
                    [<<6:?short>>, << "Value2" >>],
                    [<<4:?short>>, << "Key1" >>],
                    [<<6:?short>>, << "Value1" >>]]]),
     ?assertEqual(polyxena:cql_string_list([<< "Value1" >>, << "Value2" >>, << "Value3" >>]),
                  [<<3:?short>>,
                   [[<<6:?short>>, << "Value3" >>],
                    [<<6:?short>>, << "Value2" >>],
                    [<<6:?short>>, << "Value1" >>]]])
    ].

  %% add your asserts in the returned list, e.g.:
  %% [
  %%   ?assert(?MODNAME:double(2) =:= 4),
  %%   ?assertMatch({ok, Pid}, ?MODNAME:spawn_link()),
  %%   ?assertEqual("ba", ?MODNAME:reverse("ab")),
  %%   ?assertError(badarith, ?MODNAME:divide(X, 0)),
  %%   ?assertExit(normal, ?MODNAME:exit(normal)),
  %%   ?assertThrow({not_found, _}, ?MODNAME:func(unknown_object))
  %% ]
%%%.
