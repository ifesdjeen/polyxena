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
     {encoding, [{repeat, 1}],
      [test_encode_string
       , test_encode_map
       , test_encode_string_list
      ]}
     , {schema, [{repeat, 1}],
        [create_drop_keyspace_test
        ]}
     , {data_types, [{repeat, 1}],
        [
         boolean_field_test
         , double_field_test
         , float_field_test
        ]}
    ].

all() -> [{group, encoding}
          , {group, schema}
          , {group, data_types}
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
    polyxena_sup:start_link(),
    {ok,{schema_change,created,"polyxena_001",[]}} = polyxena:execute_cql(pool1, "CREATE KEYSPACE \"polyxena_001\" WITH replication = {'class' : 'SimpleStrategy', 'replication_factor' : 1};"),
    {ok,{schema_change, dropped,"polyxena_001",[]}} = polyxena:execute_cql(pool1, "DROP KEYSPACE \"polyxena_001\";").

boolean_field_test(_Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_boolean\" (pk, f) VALUES (1, true);"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_boolean\" (pk, f) VALUES (2, false);"),
    Result = polyxena:execute_cql(pool1, "SELECT * FROM test_boolean;"),
    io:format("asd ~p", [Result]),
    {ok, [[{"f",false},{"pk",2}],[{"f",true},{"pk",1}]]} = Result.

double_field_test(_Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_double\" (pk, f) VALUES (1, 1.123);"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_double\" (pk, f) VALUES (2, 5.678);"),
    Result = polyxena:execute_cql(pool1, "SELECT * FROM test_double;"),
    {ok,[[{"f",5.678},{"pk",2}],[{"f",1.123},{"pk",1}]]} = Result.

float_field_test(_Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_float\" (pk, f) VALUES (1, 1.123);"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_float\" (pk, f) VALUES (2, 5.678);"),
    Result = polyxena:execute_cql(pool1, "SELECT * FROM test_float;"),
    {ok,[[{"f",5.678},{"pk",2}],[{"f",1.123},{"pk",1}]]} = Result.


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



init_per_group(data_types, _Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "CREATE TABLE \"test_boolean\" (pk int, f boolean, PRIMARY KEY (pk));"),
    polyxena:execute_cql(pool1, "CREATE TABLE \"test_double\" (pk int, f double, PRIMARY KEY (pk));"),
    polyxena:execute_cql(pool1, "CREATE TABLE \"test_float\" (pk int, f double, PRIMARY KEY (pk));"),
    _Config;

init_per_group(_Group, _Config) ->
    _Config.

end_per_group(data_types, _Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_boolean\";"),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_double\";"),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_float\";"),
    _Config;

end_per_group(_Group, _Config) ->
    _Config.
