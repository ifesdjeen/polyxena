%%% HEADER
%%% @author Alex P <alex@clojurewerkz.org>
%%% @since
%%% @copyright 2014 Alex P
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
         , decimal_field_test
         , text_field_test
         , timestamp_field_test
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

to_imprecise_32_float(Float) ->
    Binary = <<Float:32/float>>,
    polyxena_connection:bytes_to_type(float, Binary).

float_field_test(_Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_float\" (pk, f) VALUES (1, 1.123);"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_float\" (pk, f) VALUES (2, 5.678);"),
    F1 = to_imprecise_32_float(5.678),
    F2 = to_imprecise_32_float(1.123),
    Result = polyxena:execute_cql(pool1, "SELECT * FROM test_float;"),
    {ok,[[{"f",F1},{"pk",2}],
         [{"f",F2},{"pk",1}]]} = Result.

to_decimal(Scale, Value) ->
    math:pow(10, Scale) * Value.

decimal_field_test(_Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_decimal\" (pk, f) VALUES (1, 1.123);"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_decimal\" (pk, f) VALUES (2, 5.678);"),
    F1 = to_decimal(3, 5678),
    F2 = to_decimal(3, 1123),
    Result = polyxena:execute_cql(pool1, "SELECT * FROM test_decimal;"),
    {ok,[[{"f",F1},{"pk",2}],
         [{"f",F2},{"pk",1}]]} = Result.

text_field_test(_Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_text\" (pk, f) VALUES (1, 'asd');"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_text\" (pk, f) VALUES (2, 'bsd');"),
    Result = polyxena:execute_cql(pool1, "SELECT * FROM test_text;"),
    {ok,[[{"f","bsd"},{"pk",2}]
         ,[{"f","asd"},{"pk",1}]]} = Result.

timestamp_field_test(_Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_timestamp\" (pk, f) VALUES (1, 1401745213794);"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_timestamp\" (pk, f) VALUES (2, 1401745227973);"),
    Result = polyxena:execute_cql(pool1, "SELECT * FROM test_timestamp;"),
    {ok,[[{"f", 1401745227973},{"pk",2}]
         ,[{"f", 1401745213794},{"pk",1}]]} = Result.

list_field_test(_Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_list\" (pk, f) VALUES (1, [1,2,3]);"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_list\" (pk, f) VALUES (2, [5,6,7]);"),
    Result = polyxena:execute_cql(pool1, "SELECT * FROM test_list;"),
    {ok,[[{"f", [5,6,7]},{"pk",2}]
         ,[{"f", [1,2,3]},{"pk",1}]]} = Result.


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
    polyxena:execute_cql(pool1, "CREATE TABLE \"test_float\" (pk int, f float, PRIMARY KEY (pk));"),
    polyxena:execute_cql(pool1, "CREATE TABLE \"test_decimal\" (pk int, f decimal, PRIMARY KEY (pk));"),
    polyxena:execute_cql(pool1, "CREATE TABLE \"test_text\" (pk int, f text, PRIMARY KEY (pk));"),
    polyxena:execute_cql(pool1, "CREATE TABLE \"test_timestamp\" (pk int, f timestamp, PRIMARY KEY (pk));"),
    polyxena:execute_cql(pool1, "CREATE TABLE \"test_list\" (pk int, f list<int>, PRIMARY KEY (pk));"),
    _Config;

init_per_group(_Group, _Config) ->
    _Config.

end_per_group(data_types, _Config) ->
    polyxena_sup:start_link(),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_boolean\";"),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_double\";"),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_float\";"),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_decimal\";"),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_text\";"),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_timestamp\";"),
    polyxena:execute_cql(pool1, "DROP TABLE \"test_list\";"),
    _Config;

end_per_group(_Group, _Config) ->
    _Config.
