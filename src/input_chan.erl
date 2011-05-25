-module(input_chan).
-behavior(gen_fsm).

-export([start_link/1, init/1]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([connecting/2]).

-include("fuse.hrl").

-record(state, {fd, port}).

start_link(Fd) ->
    gen_fsm:start_link(?MODULE, Fd, []).

init(Fd) ->
    Port = erlang:open_port({fd, Fd, Fd}, [stream, binary]),
    {ok, connecting, #state{fd=Fd, port=Port}}.

handle_event(Event, _StateName, _State) -> 
    erlang:throw({"Bad event", Event}).

handle_sync_event(Event, _From, _StateName, _State) -> 
    erlang:throw({"Bad event", Event}).

handle_info({Port, {data, Data}}, StateName, State) ->
    io:format("Data: ~p (~p)~n", [Data, size(Data)]),
    ?MODULE:StateName(Data, State).

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

decode_in_header(<<?IN_HEADER_SIZE:32/native,
		   OpCode:32/native,
		   Unique:64/native,
		   NodeId:64/native,
		   Uid:32/native,
		   Gid:32/native,
		   Pid:32/native,
		   _T/binary>>=Data)
  when size(Data) =:= ?IN_HEADER_SIZE ->
    #in_header{opcode=OpCode, unique=Unique, nodeid=NodeId, uid=Uid, gid=Gid, pid=Pid}.

connecting(Data, State) when is_binary(Data) ->
    connecting(decode_in_header(Data), State);
connecting(#in_header{opcode=?FUSE_INIT}=Header, State) ->
    io:format("Initialized~n", []),
    {next_state, connecting, State}.
