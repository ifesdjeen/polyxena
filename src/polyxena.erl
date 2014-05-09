-module(polyxena).

%% polyxena: polyxena library's entry point.

-export([
         init_client/2
         , cql_string/1
         , cql_map/1]).

-compile(export_all).

-define(CQL_VERSION, <<"3.0.0">>).

-define(OPCODE_STARTUP,     1).

-define(short, 1/big-unsigned-unit:16).

cql_string(Str) ->
    Size = size(Str),
    [<< Size:?short >>, Str].

cql_map(Map) ->
    {Length, Binaries} =
        lists:foldl(fun({Key, Value}, {Index, Acc}) ->
                            {Index + 1, [cql_string(Key), cql_string(Value) | Acc]}
                    end, {0, []}, Map),
    [<<Length:?short>>, Binaries].

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
