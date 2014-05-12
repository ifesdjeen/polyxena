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

-define(FRAME_TYPE_REQUEST,    2#0).
-define(FRAME_TYPE_RESPONSE,   2#1).

-define(FRAME_VERSION,         2#0000010).

-define(frame_part_type, 1/unsigned-integer).
-define(frame_part_version, 7/unsigned-integer).
-define(frame_part_flags, 8/unsigned-integer).
-define(frame_part_opcode, 8/unsigned-integer).
-define(frame_part_stream, 8/signed-integer).

-define(short, 1/big-unsigned-unit:16).
-define(int,   1/big-signed-unit:32).
-define(long,  1/big-signed-unit:64).


-record(frame, {
          type     = request,
          version  = ?FRAME_VERSION,
          flags    = 0,
          stream   = 1,
          opcode,
          length,
          body =   <<>>
         }).


type_to_atom(?FRAME_TYPE_REQUEST)  -> request;
type_to_atom(?FRAME_TYPE_RESPONSE) -> response.

type_from_atom(request)            -> ?FRAME_TYPE_REQUEST;
type_from_atom(response)           -> ?FRAME_TYPE_RESPONSE.


opcode_to_atom(?OPCODE_ERROR)             -> error;
opcode_to_atom(?OPCODE_STARTUP)           -> startup;
opcode_to_atom(?OPCODE_READY)             -> ready;
opcode_to_atom(?OPCODE_AUTHENTICATE)      -> authenticate;
opcode_to_atom(?OPCODE_OPTIONS)           -> options;
opcode_to_atom(?OPCODE_SUPPORTED)         -> supported;
opcode_to_atom(?OPCODE_QUERY)             -> query;
opcode_to_atom(?OPCODE_PREPARE)           -> prepare;
opcode_to_atom(?OPCODE_EXECUTE)           -> execute;
opcode_to_atom(?OPCODE_REGISTER)          -> register;
opcode_to_atom(?OPCODE_EVENT)             -> event;
opcode_to_atom(?OPCODE_BATCH)             -> batch;
opcode_to_atom(?OPCODE_AUTH_CHALLENGE)    -> auth_challenge;
opcode_to_atom(?OPCODE_AUTH_RESPONSE)     -> auth_response;
opcode_to_atom(?OPCODE_AUTH_SUCCESS)      -> auth_success.
