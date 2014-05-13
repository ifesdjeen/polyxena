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

decode_cql_string(<<Length:?short, Str:Length/binary-unit:8, _/binary>>) ->
    binary_to_list(Str).

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
            {ok}
    end.


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
    gen_tcp:send(Socket, EncodedFrame).

startup_frame() ->
    #frame{opcode = ?OPCODE_STARTUP,
           body = cql_map([{<<"CQL_VERSION">>, ?CQL_VERSION}])
          }.

query_frame(Query, Consistency) when is_list(Query) ->
    query_frame(list_to_binary(Query), Consistency);

query_frame(Query, Consistency) ->
    #frame{opcode = ?OPCODE_QUERY,
           body = [cql_long_string(Query), cql_short(Consistency)]}.

establish_connection(Host, Port) ->
    Socket = polyxena:init_client(Host, Port),
    send_frame(Socket, startup_frame()),
    case receive_frame(Socket) of
        {ok} ->
            Socket
    end.

%% End of Module.

tryout() ->
    Socket = establish_connection("192.168.60.15", 9042),
    send_frame(Socket, query_frame("asdasd;", ?CONSISTENCY_ONE)),
    io:format("Response: ~w  ~n", receive_frame(Socket)).

    %% polyxena:tryout().
