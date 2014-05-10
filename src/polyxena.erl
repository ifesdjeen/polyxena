-module(polyxena).

%% polyxena: polyxena library's entry point.

-export([
         init_client/2
         , cql_string/1
         , cql_string_list/1
         , cql_map/1]).

-compile(export_all).

-include("cqldefs.hrl").

cql_int(Int)      when is_integer(Int)     -> <<Int:?int>>.
cql_short(Short)  when is_integer(Short)   -> <<Short:?short>>.
cql_long(Long)    when is_integer(Long)    -> <<Long:?long>>.

cql_string(Str) when is_binary(Str) ->
    Size = size(Str),
    [cql_short(Size), Str].

cql_long_string(Str) when is_binary(Str) ->
    Size = size(Str),
    [cql_int(Size), Str].

cql_string_list(List) when is_list(List) ->
    Strings = lists:foldl(fun(Item, Acc) ->
                                  [cql_string(Item) | Acc]
                          end, [], List),
    [cql_short(length(List)), Strings].


cql_map(Map) when is_list(Map) ->
    {Length, Binaries} =
        lists:foldl(fun({Key, Value}, {Index, Acc}) ->
                            {Index + 1, [cql_string(Key), cql_string(Value) | Acc]}
                    end, {0, []}, Map),
    [cql_short(Length), Binaries].



%% API

init_client(Host, Port) ->
    Opts = [
            {active, false},
            {packet, raw},
            binary,
            {nodelay, true}
           ],
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, RemoteSock} ->
            io:format("Connected to remote host ~p:~p ~n ~w", Host, Port),
            RemoteSock;
        {error, Why} ->
            io:format("Can't connect to remote: '~w'~n", [Why])
    end.


%% End of Module.
