-module(input_chan).
-behavior(gen_fsm).

-export([start_link/3, init/1]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-import(output_chan, [send/2]).

-export([connecting/1, idle/1]).

-include("fuse.hrl").
-include("errno.hrl").

-record(state, {fd, watcher, handler, cookie, decoder,
		major, minor, flags, tracer, opid}).

start_link(MountPoint, Handler, Cookie) ->
    gen_fsm:start_link(?MODULE, [MountPoint, Handler, Cookie], []).

tracer() ->
    receive
	Msg ->
	    io:format("TRACE: ~p~n", [Msg])
    end,
    tracer().

init([MountPoint, Handler, Cookie]) ->
    {ok, Fd} = fuse:mount(MountPoint),
    Tracer = spawn_link(fun() -> tracer() end),
    {ok, Watcher} = procket:watcher_create(Fd, 1, []),
    {ok, connecting, #state{fd=Fd, watcher=Watcher, handler=Handler,
			    cookie=Cookie, tracer=Tracer}}.

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
    {ok, Pid} =
	efuse_mount_sup:start_child({erlang:make_ref(),
				     {output_chan,
				      start_link,
				      [State#state.fd]},
				     transient,
				     brutal_kill,
				     worker,
				     [output_chan]
				    }),
    {Header, Payload} = recv(State),
    #in_header{len=?IN_HEADER_SIZE+?INIT_IN_SIZE,
	       opcode=?FUSE_INIT} = Header,
    InitIn = fuse_packet:init_in(Payload),
    negotiate(InitIn#init_in.major, Header, InitIn, State#state{opid=Pid}).

% 
% Version negotiation:
% 
% Both the kernel and userspace send the version they support in the
% INIT request and reply respectively.
% 
% If the major versions match then both shall use the smallest
% of the two minor versions for communication.
% 
% If the kernel supports a larger major version, then userspace shall
% reply with the major version it supports, ignore the rest of the
% INIT message and expect a new INIT message from the kernel with a
% matching major version.
% 
% If the library supports a larger major version, then it shall fall
% back to the major protocol version sent by the kernel for
% communication and reply with that major version (and an arbitrary
% supported minor version).
% 
negotiate(?API_MAJOR, Header, InitIn, State) ->
    Minor = erlang:min(InitIn#init_in.minor, ?API_MINOR),
    FlagsRequested = InitIn#init_in.flags,
    Flags = 0, % FIXME
    error_logger:info_msg("FUSE: Connection established, API: ~p.~p, Flags: ~p~n",
			  [?API_MAJOR, Minor, Flags]),

    ok = output_chan:set_compat(State#state.opid,
				{?API_MAJOR, Minor},
				Flags),
    Decoder = decoder:init(State#state.handler, State#state.cookie,
			   State#state.opid, {?API_MAJOR, Minor}, Flags),
    send(State#state.opid,
	 [
	  #out_header{error=0, unique=Header#in_header.unique},
	  #init_out{major=?API_MAJOR,
		    minor=Minor,
		    max_readahead=InitIn#init_in.max_readahead,
		    flags=Flags,
		    max_background=65536,
		    congestion_threshold=60000,
		    max_write=65536
		   }
	 ]),

    NewState = State#state{decoder=Decoder,
			   major=?API_MAJOR,
			   minor=Minor,
			   flags=Flags},

    {next_state, idle, NewState};
negotiate(Major, Header, InitIn, State) when Major > ?API_MAJOR ->
    error_logger:info_msg("FUSE: Kernel is too new (API: ~p.~p), renegotiate~n",
			  [Major, InitIn#init_in.minor]),
    send(State#state.opid,
	 [
	  #out_header{error=0, unique=Header#in_header.unique},
	  #init_out{major=?API_MAJOR,
		    minor=?API_MINOR
		   }
	 ]),
    {next_state, connecting, State}; % remain in connecting state
negotiate(_Major, Header, _InitIn, State) ->
    send(State#state.opid,
	 #out_header{error=-?EPROTO, unique=Header#in_header.unique}),
    {stop, "Incompatible Protocol", State}.

idle(State) ->
    {Header, Payload} = recv(State),
    spawn_link(
      fun() ->
	      %erlang:trace(self(), true, [call, {tracer, State#state.tracer}]),
	      %erlang:trace_pattern({decoder, '_', '_'}, true, [local]),

	      decoder:process(Header, Payload, State#state.decoder)
      end
     ),
    {next_state, idle, State}.

recv(State, Len) ->
    {ok, Data} = procket:read(State#state.fd, Len),
    io:format("RECV: ~p~n", [Data]),
    Data.

recv(State) ->
    RawData = recv(State, 65536),
    RawHeader = binary:part(RawData, {0, ?IN_HEADER_SIZE}),
    Header = fuse_packet:in_header(RawHeader),
    Payload = if
		  Header#in_header.len =:= size(RawData) ->
		      Remain = Header#in_header.len - ?IN_HEADER_SIZE,
		      binary:part(RawData, {?IN_HEADER_SIZE, Remain})
	      end,
    {Header, Payload}.



