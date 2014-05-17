-module(polyxena).

%% polyxena: polyxena library's entry point.

-export([
         init_client/2
         , has_flag/2
         , cql_encode/2
        ]).

-compile(export_all).

-include("cqldefs.hrl").

has_flag(Flag, [Flag | _])   -> true;
has_flag(Flag, [_ | More])   -> has_flag(Flag, More);
has_flag(_, [])              -> false.

%%
%% Encoding
%%

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



%%
%% DECODING
%%

decode_cql_string(<<Length:?short, Str:Length/binary-unit:8, _/binary>>) ->
    binary_to_list(Str).


%%
%% CONSUMPTION
%%

%% Decode functions may be just like consume functions just dropping out the tail
consume(string, <<Length:?short, Str:Length/binary-unit:8, Rest/binary>>) ->
    {Str, Rest};

consume(int, <<Int:?int, Rest/binary>>) ->
    {Int, Rest}.

consume_rows_flags(<<Flags:?int, Rest/binary>>) ->
    Res = lists:foldl(fun({Flag, Mask}, Acc) ->
                              if Flags band Mask == Mask -> [Flag | Acc];
                                 true -> Acc
                              end
                      end, [], [{global_tables_spec, 1}]),
    {Res, Rest}.

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
%% basic iterations, such as partition, repeat, group by and so on?
%% check out that emacs library, maybe it'll be some use here? Or maybe
%% there is an equivalent already, which would be even better.
%% How to call anonymous functions??

decode_result_kind(?RESULT_KIND_ROWS, Binary) ->
%% <<Flags0:?int,
%%                                         ColumnsCount:?int,
%%                                         Rest/binary>>) ->
    {Flags, Rest0} = consume_rows_flags(Binary),
    {ColumnsCount, Rest1} = consume(int, Rest0),
    HasGlobalTablesSpec = has_flag(global_tables_spec, Flags),
    if HasGlobalTablesSpec ->
            {Keyspace, Rest2}    = consume(string, Rest1),
            {Table, Rest3}       = consume(string, Rest2),
            {ColumnSpecs, Rest4} = decode_col_specs(ColumnsCount, Rest3),
            {binary_to_list(Keyspace), binary_to_list(Table), ColumnSpecs};
        true -> []
    end.
%% {Flags, ColumnsCount, Rest}.

decode_col_specs(Remaining, Binary) -> decode_col_specs(Remaining, Binary, []).

decode_col_specs(Remaining, Binary, Acc) ->
    if Remaining > 0 ->
            {ColumnName, Rest0} = consume(string, Binary),
            {Option, Rest1}     = decode_col_spec(Rest0),
            decode_col_specs(Remaining - 1, Rest1, [{ColumnName, Option} | Acc]);
       Remaining == 0 -> {Acc, Binary}
    end.

decode_col_spec(<<0:?short,
                  Length:?short,
                  CustomTypeName:Length/binary-unit:8,
                  Rest/binary>>) ->
    {{custom, CustomTypeName}, Rest};

decode_col_spec(<<16#1:?short, Rest/binary>>) -> {ascii, Rest};
decode_col_spec(<<16#2:?short, Rest/binary>>) -> {bigint, Rest};
decode_col_spec(<<16#3:?short, Rest/binary>>) -> {blob, Rest};
decode_col_spec(<<16#4:?short, Rest/binary>>) -> {boolean, Rest};
decode_col_spec(<<16#5:?short, Rest/binary>>) -> {counter, Rest};
decode_col_spec(<<16#6:?short, Rest/binary>>) -> {decimal, Rest};
decode_col_spec(<<16#7:?short, Rest/binary>>) -> {double, Rest};
decode_col_spec(<<16#8:?short, Rest/binary>>) -> {float, Rest};
decode_col_spec(<<16#9:?short, Rest/binary>>) -> {int, Rest};
decode_col_spec(<<16#A:?short, Rest/binary>>) -> {text, Rest};
decode_col_spec(<<16#B:?short, Rest/binary>>) -> {timestamp, Rest};
decode_col_spec(<<16#C:?short, Rest/binary>>) -> {uuid, Rest};
decode_col_spec(<<16#D:?short, Rest/binary>>) -> {varchar, Rest};
decode_col_spec(<<16#E:?short, Rest/binary>>) -> {varint, Rest};
decode_col_spec(<<16#F:?short, Rest/binary>>) -> {timeuuid, Rest};
decode_col_spec(<<16#10:?short, Rest/binary>>) -> {inet, Rest};

decode_col_spec(<<16#20:?short, Rest/binary>>) ->
    {SubType, Rest1} = decode_col_spec(Rest),
    {list, SubType, Rest1};

decode_col_spec(<<16#21:?short, Rest/binary>>) ->
    {SubType, Rest1} = decode_col_spec(Rest),
    {map, SubType, Rest1};

decode_col_spec(<<16#22:?short, Rest/binary>>) ->
    {SubType, Rest1} = decode_col_spec(Rest),
    {set, SubType, Rest1}.

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
           body = cql_encode(map, [{<<"CQL_VERSION">>, ?CQL_VERSION}])
          }.

query_frame(Query, Consistency) when is_list(Query) ->
    query_frame(list_to_binary(Query), Consistency);

query_frame(Query, Consistency) ->
    #frame{opcode = ?OPCODE_QUERY,
           body = [cql_encode(long_string, Query),
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
