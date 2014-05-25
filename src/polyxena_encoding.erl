-module(polyxena_encoding).

-export([cql_encode/2
         ]).
-include("include/cqldefs.hrl").


cql_encode(int, Int)      when is_integer(Int)     -> <<Int:?int>>;

cql_encode(short, Short)  when is_integer(Short)   -> <<Short:?short>>;

cql_encode(long, Long)    when is_integer(Long)    -> <<Long:?long>>;

cql_encode(string, Str)   when is_binary(Str) ->
    [cql_encode(short, size(Str)), Str];

cql_encode(long_string, Str) when is_binary(Str) ->
    Size = size(Str),
    [cql_encode(int, Size), Str];

cql_encode(string_list, List) when is_list(List) ->
    Strings = lists:foldl(fun(Item, Acc) ->
                                  [cql_encode(string, Item) | Acc]
                          end, [], List),
    [cql_encode(short, length(List)), Strings];

cql_encode(map, Map) when is_list(Map) ->
    {Length, Binaries} =
        lists:foldl(fun({Key, Value}, {Index, Acc}) ->
                            {Index + 1, [cql_encode(string, Key),
                                         cql_encode(string, Value) | Acc]}
                    end, {0, []}, Map),
    [cql_encode(short, Length), Binaries].
