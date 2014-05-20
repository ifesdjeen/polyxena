%%%'   HEADER
%%% @author Alex P <alex@clojurewerkz.org>
%%% @since
%%% @copyright 2014 Susan Potter
%%% @doc
%%% @end
-module(basic_SUITE).

-export([all/0]).
-export([test_encode_string/1,
         test_encode_map/1,
         test_encode_string_list/1
        ]).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-include("../src/cqldefs.hrl").



%%%.
%%%' CALLBACKS

all() -> [test_encode_string, test_encode_map, test_encode_string_list].

test_encode_string(_Config) ->
    Result = polyxena:cql_encode(string, << "String" >>),
    Result = [<<6:?short>>, << "String" >>].

test_encode_map(_Config) ->
    Result = polyxena:cql_encode(map, [{<< "Key1" >>, << "Value1" >>},
                                       {<< "Key2" >>, << "Value2" >>}]),
    Result = [<<2:?short>>,
               [[<<4:?short>>, << "Key2" >>],
                [<<6:?short>>, << "Value2" >>],
                [<<4:?short>>, << "Key1" >>],
                [<<6:?short>>, << "Value1" >>]]].

test_encode_string_list(_Config) ->
    Result = polyxena:cql_encode(string_list,
                                 [<< "Value1" >>, << "Value2" >>, << "Value3" >>]),
    Result = [<<3:?short>>,
              [[<<6:?short>>, << "Value3" >>],
               [<<6:?short>>, << "Value2" >>],
               [<<6:?short>>, << "Value1" >>]]].
