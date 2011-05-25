-module(input_chan).
-behavior(gen_fsm).

-export([start_link/1, init/1]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {fd, port}).

start_link(Fd) ->
    gen_fsm:start_link(?MODULE, Fd, []).

init(Fd) ->
    Port = erlang:open_port({fd, Fd, Fd}, [stream, binary]),
    {ok, idle, #state{fd=Fd, port=Port}}.

handle_event(Event, _StateName, _State) -> 
    erlang:throw({"Bad event", Event}).

handle_sync_event(Event, _From, _StateName, _State) -> 
    erlang:throw({"Bad event", Event}).

handle_info({Port, {data, Data}}, idle, State) ->
    io:format("Info: ~p~n", [Data]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.
