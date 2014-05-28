-define(CQL_VERSION, <<"3.0.0">>).

%%
%% Opcodes
%%

-define(OPCODE_ERROR,          0).
-define(OPCODE_STARTUP,        1).
-define(OPCODE_READY,          2).
-define(OPCODE_AUTHENTICATE,   3).
-define(OPCODE_CREDENTIALS,    4).
-define(OPCODE_OPTIONS,        5).
-define(OPCODE_SUPPORTED,      6).
-define(OPCODE_QUERY,          7).
-define(OPCODE_RESULT,         8).
-define(OPCODE_PREPARE,        9).
-define(OPCODE_EXECUTE,        10).
-define(OPCODE_REGISTER,       11).
-define(OPCODE_EVENT,          16#0C).
-define(OPCODE_AUTH_RESPONSE,  13).
-define(OPCODE_AUTH_SUCCESS,   14).

-define(CONSISTENCY_ANY,           0).
-define(CONSISTENCY_ONE,           1).
-define(CONSISTENCY_TWO,           2).
-define(CONSISTENCY_THREE,         3).
-define(CONSISTENCY_QUORUM,        4).
-define(CONSISTENCY_ALL,           5).
-define(CONSISTENCY_LOCAL_QUORUM,  6).
-define(CONSISTENCY_EACH_QUORUM,   7).
-define(CONSISTENCY_LOCAL_ONE,     10).

-define(RESULT_KIND_VOID,               10#1).
-define(RESULT_KIND_ROWS,               10#2).
-define(RESULT_KIND_SET_KEYSPACE,       10#3).
-define(RESULT_KIND_PREPARED,           10#4).
-define(RESULT_KIND_SCHEMA_CHANGE,      10#5).

-define(FRAME_TYPE_REQUEST,    2#0).
-define(FRAME_TYPE_RESPONSE,   2#1).

-define(FRAME_VERSION,         2#0000001).

-define(frame_part_type, 1/unsigned-integer).
-define(frame_part_version, 7/unsigned-integer).
-define(frame_part_stream, 8/signed-integer).
-define(frame_part_flags, 8/unsigned-integer).
-define(frame_part_opcode, 8/unsigned-integer).


-define(short,  1/big-unsigned-unit:16).
-define(int,    1/big-signed-unit:32).
-define(long,   1/big-signed-unit:64).
-define(bigint, 1/big-signed-unit:64).


-record(frame, {
          type     = ?FRAME_TYPE_REQUEST,
          version  = ?FRAME_VERSION,
          flags    = 0,
          stream   = 1,
          opcode,
          length,
          body =   <<>>
         }).

%%
%% Debug-only
%%

%% type_to_atom(?FRAME_TYPE_REQUEST)  -> request;
%% type_to_atom(?FRAME_TYPE_RESPONSE) -> response.
