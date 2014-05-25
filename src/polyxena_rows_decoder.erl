-module(polyxena_rows_decoder).

-export([decode_rows/5
        ]).

-compile(export_all).

-include("include/cqldefs.hrl").

decode_rows(RemainingRows, Binary, ColumnsCount, ColumnSpecs, Acc) ->
    if RemainingRows > 0 ->
            {Row, Remaining} = decode_row(ColumnsCount, Binary, ColumnSpecs),
            decode_rows(RemainingRows - 1, Remaining, ColumnsCount, ColumnSpecs,
                        [Row | Acc]);
       RemainingRows == 0 -> {Acc, Binary}
    end.

decode_row(Remaining, Binary, ColumnSpecs) ->
    decode_row(Remaining, Binary, ColumnSpecs, []).

decode_row(Remaining, Binary, ColumnSpecs, Acc) ->
    if Remaining > 0 ->
            {ColumnNameRaw, ColumnType}   = lists:nth(Remaining, ColumnSpecs),
            ColumnName                    = column_name_to_str(ColumnNameRaw),
            {DecodedRow, RemainingBytes}  = consume_bytes(Binary),
            Value                         = bytes_to_type(ColumnType, DecodedRow),
            decode_row(Remaining - 1, RemainingBytes,
                       ColumnSpecs, [{ColumnName, Value} | Acc]);
       Remaining == 0 ->
            {Acc, Binary}
    end.

consume_bytes(<<Length:?int, Str:Length/binary-unit:8, Rest/binary>>) ->
    {Str, Rest}.

bytes_to_type({custom, _}, Bytes) -> Bytes;
bytes_to_type(ascii, <<Bytes/binary>>)       -> binary_to_list(Bytes);
bytes_to_type(varchar, <<Bytes/binary>>)     -> binary_to_list(Bytes);
bytes_to_type(int, <<Int:?int>>)  -> Int.


column_name_to_str(<<Bytes/binary>>)         -> binary_to_list(Bytes).
