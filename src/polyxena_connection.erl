-module(polyxena_connection).

-export([
         connect/2
         , has_flag/2
        ]).

-compile(export_all).

-include("include/cqldefs.hrl").

%%
%% Impl
%%

has_flag(Flag, [Flag | _])   -> true;
has_flag(Flag, [_ | More])   -> has_flag(Flag, More);
has_flag(_, [])              -> false.

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
    {Int, Rest};

consume(rows_flags, <<Flags:?int, Rest/binary>>) ->
    Res = lists:foldl(fun({Flag, Mask}, Acc) ->
                              if Flags band Mask == Mask -> [Flag | Acc];
                                 true -> Acc
                              end
                      end, [], [{global_tables_spec, 1}]),
    {Res, Rest};

consume({col_specs, ColSpecCount}, Binary) -> decode_col_specs(ColSpecCount, Binary, []);

consume({rows, RowsCount, ColumnsCount, ColumnSpecs}, Binary) ->
    polyxena_rows_decoder:decode_rows(RowsCount, Binary, ColumnsCount, ColumnSpecs, []).

decode_col_specs(Remaining, Binary, Acc) ->
    if Remaining > 0 ->
            {ColumnName, Rest0} = consume(string, Binary),
            {Option, Rest1}     = decode_col_spec(Rest0),
            decode_col_specs(Remaining - 1, Rest1,
                             [{ColumnName, Option} | Acc]);
       Remaining == 0 -> {Acc, Binary}
    end.


%%
%% Decoding
%%

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

decode_result_kind(?RESULT_KIND_ROWS, Binary) ->
    {[Flags, ColumnsCount], Rest1} = consume_specs(Binary, [rows_flags, int]),
    HasGlobalTablesSpec   = has_flag(global_tables_spec, Flags),
    if HasGlobalTablesSpec ->
            {[Keyspace, Table,
              ColumnSpecs, RowsCount], Rest5} = consume_specs(Rest1, [string,
                                                                      string,
                                                                      {col_specs, ColumnsCount},
                                                                      int]),
            {Rows, Rest6}        = consume({rows, RowsCount, ColumnsCount, ColumnSpecs}, Rest5),
            {binary_to_list(Keyspace),
             binary_to_list(Table),
             ColumnSpecs,
             RowsCount,
             Rows};
       true -> []
    end.

%% {Flags, ColumnsCount, Rest}.

consume_specs(Binary, Spec) ->
    {Result, Rest} = lists:foldl(fun(Item, {Acc, CurrentBinary}) ->
                                         {Decoded, Rest} = consume(Item, CurrentBinary),
                                         {[Decoded | Acc], Rest}
                                 end, {[], Binary}, Spec),
    {lists:reverse(Result), Rest}.

decode_col_specs(Remaining, Binary) -> decode_col_specs(Remaining, Binary, []).

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

connect(Host, Port) ->
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

send_frame(Socket, Frame) ->
    EncodedFrame = frame_to_binary(Frame),
    %% io:format("SEND: ~p\n",[iolist_to_binary(EncodedFrame)]),
    gen_tcp:send(Socket, EncodedFrame).

startup_frame() ->
    #frame{opcode = ?OPCODE_STARTUP,
           body = polyxena_encoding:cql_encode(map, [{<<"CQL_VERSION">>, ?CQL_VERSION}])
          }.

query_frame(Query, Consistency) when is_list(Query) ->
    query_frame(list_to_binary(Query), Consistency);

query_frame(Query, Consistency) ->
    #frame{opcode = ?OPCODE_QUERY,
           body = [polyxena_encoding:cql_encode(long_string, Query),
                   polyxena_encoding:cql_encode(short, Consistency)
                  ]}.

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

establish_connection(Host, Port) ->
    Socket = connect(Host, Port),
    send_frame(Socket, startup_frame()),
    case receive_frame(Socket) of
        {ready} ->
            Socket
    end.

execute_cql(Connection, Query) ->
    send_frame(Connection, query_frame(Query, ?CONSISTENCY_ONE)),
    receive_frame(Connection).
