-module(input_chan).
-behavior(gen_fsm).

-export([start_link/1, init/1]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([connecting/1]).

-include("fuse.hrl").

-record(state, {fd, watcher}).

start_link(Fd) ->
    gen_fsm:start_link(?MODULE, Fd, []).

init(Fd) ->
    {ok, Watcher} = procket:watcher_create(Fd, 1, []),
    {ok, connecting, #state{fd=Fd, watcher=Watcher}}.

handle_event(Event, _StateName, _State) -> 
    erlang:throw({"Bad event", Event}).

handle_sync_event(Event, _From, _StateName, _State) -> 
    erlang:throw({"Bad event", Event}).

handle_info({procket_watcher, 1, []}, StateName, State) ->
    Ret = ?MODULE:StateName(State),
    procket:watcher_arm(State#state.watcher),
    Ret.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

connecting(State) ->
    Header = recv_header(State),
    #in_header{opcode=?FUSE_INIT, unique=Unique} = Header,
    output_chan:send(#out_header{error=0, unique=Unique}),
    io:format("Initialized~n", []),
    {next_state, connecting, State}.    

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

recv(State, Len) ->
    {ok, Data} = procket:read(State#state.fd, Len),
    Data.

recv_header(State) ->
    Header = recv(State, ?IN_HEADER_SIZE),
    decode_in_header(Header).


