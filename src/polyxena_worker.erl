-module(polyxena_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Hostname = proplists:get_value(hostname, Args),
    Port = proplists:get_value(port, Args),

    Connection = polyxena_connection:establish_connection(Hostname, Port),
    %% io:format("asdasd ~n ~w"),
    {ok, #state{conn=Connection}}.


handle_call({execute_cql, Cql}, _From, #state{conn=Conn}=State) ->
    {reply, polyxena_connection:execute_cql(Conn, Cql), State};

handle_call(_Other, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
