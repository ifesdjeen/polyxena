-module(polyxena).

%% polyxena: polyxena library's entry point.
%% API
-export([execute_cql/2]).

execute_cql(PoolName, Cql) ->
    poolboy:transaction(PoolName,
                        fun(W) ->
                                gen_server:call(W, {execute_cql, Cql})
                        end).
