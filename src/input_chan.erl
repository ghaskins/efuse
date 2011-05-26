-module(input_chan).
-behavior(gen_fsm).

-export([start_link/2, init/1]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([connecting/1, idle/1]).

-include("fuse.hrl").

-record(state, {fd, decoder, watcher, major, minor, max_readahead, flags}).

start_link(Fd, Handler) ->
    gen_fsm:start_link(?MODULE, [Fd, Handler], []).

init([Fd, Handler]) ->
    Decoder = decoder:init(Handler),
    {ok, Watcher} = procket:watcher_create(Fd, 1, []),
    {ok, connecting, #state{fd=Fd, decoder=Decoder, watcher=Watcher}}.

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
    {Header, Payload} = recv(State),
    #in_header{len=?IN_HEADER_SIZE+?INIT_IN_SIZE,
	       opcode=?FUSE_INIT,
	       unique=Unique} = Header,
    <<Major:32/native,
      Minor:32/native,
      ReadAhead:32/native,
      Flags:32/native>> = Payload,
    error_logger:info_msg("FUSE: Connection established, Major: ~p Minor: ~p~n",
			  [Major, Minor]),
    output_chan:send(#out_header{error=0, unique=Unique}),
    NewState = State#state{major=Major,
			   minor=Minor,
			   max_readahead=ReadAhead,
			   flags=Flags},
    {next_state, idle, NewState}.    

idle(State) ->
    {Header, Payload} = recv(State),
    spawn_link(
      fun() ->
	      decoder:process(Header, Payload, State#state.decoder)
      end
     ),
    {next_state, idle, State}.

decode_in_header(<<Len:32/native,
		   OpCode:32/native,
		   Unique:64/native,
		   NodeId:64/native,
		   Uid:32/native,
		   Gid:32/native,
		   Pid:32/native,
		   _T/binary>>=Data)
  when size(Data) =:= ?IN_HEADER_SIZE ->
    #in_header{len=Len, opcode=OpCode, unique=Unique,
	       nodeid=NodeId, uid=Uid, gid=Gid, pid=Pid}.

recv(State, Len) ->
    {ok, Data} = procket:read(State#state.fd, Len),
    Data.

recv(State) ->
    RawData = recv(State, 65536),
    RawHeader = binary:part(RawData, {0, ?IN_HEADER_SIZE}),
    Header = decode_in_header(RawHeader),
    Payload = if
		  Header#in_header.len =:= size(RawData) ->
		      Remain = Header#in_header.len - ?IN_HEADER_SIZE,
		      binary:part(RawData, {?IN_HEADER_SIZE, Remain})
	      end,
    {Header, Payload}.



