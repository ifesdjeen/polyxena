-module(polyxena).

%% polyxena: polyxena library's entry point.

-export([
         init_client/2
         , cql_encode/2
         , cql_string_list/1
         , cql_map/1]).

-compile(export_all).

-include("cqldefs.hrl").

cql_encode(int, Int)      when is_integer(Int)     -> <<Int:?int>>;

cql_encode(short, Short)  when is_integer(Short)   -> <<Short:?short>>;

cql_encode(long, Long)    when is_integer(Long)    -> <<Long:?long>>;

cql_encode(string, Str) when is_binary(Str) ->
    [cql_encode(short, size(Str)), Str].

decode_cql_string(<<Length:?short, Str:Length/binary-unit:8, _/binary>>) ->
    binary_to_list(Str).

cql_long_string(Str) when is_binary(Str) ->
    Size = size(Str),
    [cql_encode(int, Size), Str].

cql_string_list(List) when is_list(List) ->
    Strings = lists:foldl(fun(Item, Acc) ->
                                  [cql_encode(string, Item) | Acc]
                          end, [], List),
    [cql_encode(short, length(List)), Strings].

cql_map(Map) when is_list(Map) ->
    {Length, Binaries} =
        lists:foldl(fun({Key, Value}, {Index, Acc}) ->
                            {Index + 1, [cql_encode(string, Key),
                                         cql_encode(string, Value) | Acc]}
                    end, {0, []}, Map),
    [cql_encode(short, Length), Binaries].



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
            io:format("Connected to remote host ~p:~p ~n", [Host, Port]),
            RemoteSock;
        {error, Why} ->
            io:format("Can't connect to remote: '~w'~n", [Why])
    end.

frame_to_binary(#frame{type = Type,
                       version = Version,
                       flags   = Flags,
                       stream  = Stream,
                       opcode  = Opcode,
                       body    = Body}) ->
    Length = iolist_size(Body),
    [<<Type:?frame_part_type,
       Version:?frame_part_version,
       Flags:?frame_part_flags,
       Stream:?frame_part_stream,
       Opcode:?frame_part_opcode,
       Length:32/big-unsigned-integer>>,
     Body].

decode_body(Opcode, Body) ->
    case Opcode of
        ?OPCODE_ERROR ->
            <<ErrCode:?int, ErrMsg/binary>> = Body,
            {error, ErrCode, decode_cql_string(ErrMsg)};
        ?OPCODE_READY ->
            {ready};
        ?OPCODE_RESULT ->
            <<ResultKind:?int, Rest/binary>> = Body,
            {ok, decode_result_kind(ResultKind, Rest)}
    end.

decode_result_kind(?RESULT_KIND_SET_KEYSPACE, Body) ->
    {ok,
     decode_cql_string(Body)};

decode_result_kind(?RESULT_KIND_SCHEMA_CHANGE,
                   <<Length1:?short,
                     Str1:Length1/binary-unit:8,
                     Length2:?short,
                     Str2:Length2/binary-unit:8,
                     Length3:?short,
                     Str3:Length3/binary-unit:8,
                     _/binary>>) ->
    {schema_change,
     binary_to_list(Str1),
     binary_to_list(Str2),
     binary_to_list(Str3)};

decode_result_kind(?RESULT_KIND_VOID, _) ->
    {void};

%% QUESTION: ?? Can you conj to the tuple??
decode_result_kind(?RESULT_KIND_ROWS, <<Flags:?int,
                                        ColumnsCount:?int,
                                        Rest/binary>>) ->
    {Flags, ColumnsCount, Rest}.

decode_rows_flags(Flags) ->
    lists:foldl(fun({Flag, Mask}, Acc) ->
                        io:format("a: ~s\n",[Acc]),
                        if Flags band Mask == Mask -> [Flag | Acc];
                           true -> Acc
                        end
                end, [], [{global_tables_spec, 1}]).



receive_frame(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, <<_:?frame_part_type,          %% Type
               _:?frame_part_version,       %% Version
               _:?frame_part_flags,         %% Flags
               _:?frame_part_stream,        %% Stream
               Opcode:?frame_part_opcode,
               Length:32/big-unsigned-integer,
               Body:Length/binary-unit:8,
               _/binary>>} ->
            decode_body(Opcode, Body)
    end.

%% io:format("Type: ~p Version: ~p Flags ~p Stream: ~p  Opcode: ~p ~n Length ~p ~n Body ~p ~n",
%%           [type_to_atom(Type),
%%            Version, Flags, Stream,
%%            opcode_to_atom(Opcode),
%%            Length,
%%            decode_body(Opcode, Body)
%%            ]).


send_frame(Socket, Frame) ->
    EncodedFrame = frame_to_binary(Frame),
    %% io:format("SEND: ~p\n",[iolist_to_binary(EncodedFrame)]),
    gen_tcp:send(Socket, EncodedFrame).

startup_frame() ->
    #frame{opcode = ?OPCODE_STARTUP,
           body = cql_map([{<<"CQL_VERSION">>, ?CQL_VERSION}])
          }.

query_frame(Query, Consistency) when is_list(Query) ->
    query_frame(list_to_binary(Query), Consistency);

query_frame(Query, Consistency) ->
    #frame{opcode = ?OPCODE_QUERY,
           body = [cql_long_string(Query),
                   cql_encode(short, Consistency)
                   ]}.

establish_connection(Host, Port) ->
    Socket = polyxena:init_client(Host, Port),
    send_frame(Socket, startup_frame()),
    case receive_frame(Socket) of
        {ready} ->
            Socket
    end.

%% End of Module.

tryout() ->
    Connection = establish_connection("192.168.60.15", 9042),

    execute_cql(Connection, "DROP KEYSPACE \"cassaforte_keyspace\";"),
    execute_cql(Connection, "CREATE KEYSPACE \"cassaforte_keyspace\" WITH replication = {'class' : 'SimpleStrategy', 'replication_factor' : 1};"),
    execute_cql(Connection, "USE \"cassaforte_keyspace\";"),
    execute_cql(Connection, "CREATE TABLE \"users\" (age int, name varchar, PRIMARY KEY (name));"),
    execute_cql(Connection, "INSERT INTO \"users\" (name, age) VALUES ('Alex', 19);"),
    execute_cql(Connection, "SELECT * from users").

    %% execute_cql(Connection, "use my_keyspace;").
    %% execute_cql(Connection, "use instana2;").
    %% execute_cql(Connection, "select * from rollups limit 1;").

    %% send_frame(Socket, query_frame("use instana2;", ?CONSISTENCY_ONE)),


    %% polyxena:tryout().


execute_cql(Connection, Query) ->
    send_frame(Connection, query_frame(Query, ?CONSISTENCY_ONE)),
    receive_frame(Connection).
