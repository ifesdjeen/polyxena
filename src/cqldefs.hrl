-define(CQL_VERSION, <<"3.0.0">>).

%%
%% Opcodes
%%

-define(OPCODE_ERROR,          0).
-define(OPCODE_STARTUP,        1).
-define(OPCODE_READY,          2).
-define(OPCODE_AUTHENTICATE,   3).
-define(OPCODE_OPTIONS,        4).
-define(OPCODE_SUPPORTED,      5).
-define(OPCODE_QUERY,          6).
-define(OPCODE_PREPARE,        7).
-define(OPCODE_EXECUTE,        8).
-define(OPCODE_REGISTER,       9).
-define(OPCODE_EVENT,          10).
-define(OPCODE_BATCH,          11).
-define(OPCODE_AUTH_CHALLENGE, 12).
-define(OPCODE_AUTH_RESPONSE,  13).
-define(OPCODE_AUTH_SUCCESS,   14).

-define(FRAME_TYPE_REQUEST,   2#00000011).
-define(FRAME_TYPE_RESPONSE,  16#83).

-define(frame_part, 8/unsigned-integer).
-define(frame_part_stream, 8/signed-integer).
-define(short, 1/big-unsigned-unit:16).
-define(int,   1/big-signed-unit:32).
-define(long,  1/big-signed-unit:64).


-record(frame, {
          version  = ?FRAME_TYPE_REQUEST,
          flags    = 0,
          stream   = 1,
          opcode,
          length,
          body =   <<>>
         }).
