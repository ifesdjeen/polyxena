%%%'   HEADER
%%% @author Alex P <alex@clojurewerkz.org>
%%% @since
%%% @copyright 2014 Susan Potter
%%% @doc
%%% @end
-module(basic_SUITE).

-compile(export_all).

-export([test_encode_string/1,
         test_encode_map/1,
         test_encode_string_list/1
        ]).

-export([init_per_suite/1
         , end_per_suite/1

        ]).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-include("./../include/cqldefs.hrl").

%%%.
%%%' CALLBACKS

groups() ->
    [
     {encoding, [{repeat, 5}],
      [test_encode_string
       , test_encode_map
       , test_encode_string_list
      ]}
     , {schema, [{repeat, 5}],
        [create_drop_keyspace_test
        ]}
    ].

all() -> [{group, encoding}
          , {group, schema}
          %% test_encode_string, test_encode_map, test_encode_string_list
          %% , create_drop_keyspace_test
          ].




test_encode_string(_Config) ->
    Result = polyxena_encoding:cql_encode(string, "String"),
    Result = [<<6:?short>>, << "String" >>],
    8 = iolist_size(Result).

test_encode_int(_Config) ->
    Result = polyxena_encoding:cql_encode(int, 123456),
    Result = << 123456 >>,
    4 = iolist_size(Result).


test_encode_map(_Config) ->
    Result = polyxena_encoding:cql_encode(map, [{"Key1", "Value1"},
                                                {"Key2", "Value2"}]),
    Result = [<<2:?short>>,
               [[<<4:?short>>, << "Key2" >>],
                [<<6:?short>>, << "Value2" >>],
                [<<4:?short>>, << "Key1" >>],
                [<<6:?short>>, << "Value1" >>]]],
    (2 + (4 * 2) + (2 * 4) + (6 * 2)) = iolist_size(Result).

test_encode_string_list(_Config) ->
    Result = polyxena_encoding:cql_encode(string_list,
                                          ["Value1",
                                           "Value2",
                                           "Value3"]),
    Result = [<<3:?short>>,
              [[<<6:?short>>, << "Value3" >>],
               [<<6:?short>>, << "Value2" >>],
               [<<6:?short>>, << "Value1" >>]]].

create_drop_keyspace_test(_Config) ->
    %% application:set_env(polyxena, pools,
    %%                     [{pool1, [{size, 10}, {max_overflow, 0}],
    %%                       [{hostname, "192.168.60.15"},
    %%                        {port, 9042},
    %%                        {keyspace, "cassaforte_keyspace"}
    %%                       ]}]),

    %% application:start(polyxena),
    polyxena_sup:start_link(),
    {ok,{schema_change,created,"polyxena_001",[]}} = polyxena:execute_cql(pool1, "CREATE KEYSPACE \"polyxena_001\" WITH replication = {'class' : 'SimpleStrategy', 'replication_factor' : 1};"),
    {ok,{schema_change, dropped,"polyxena_001",[]}} = polyxena:execute_cql(pool1, "DROP KEYSPACE \"polyxena_001\";").

init_per_suite(_Config) ->
    application:set_env(polyxena, pools,
                        [{pool1, [{size, 10}, {max_overflow, 0}],
                          [{hostname, "192.168.60.15"},
                           {port, 9042},
                           {keyspace, "cassaforte_keyspace"}
                          ]}]),

    application:start(polyxena),
    _Config.

end_per_suite(_Config) ->
    application:start(polyxena),
    ok.

%% suite() ->
%%     [{timetrap,{seconds,100}}].
