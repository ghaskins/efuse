-module(chan).
-behavior(gen_server).

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {fd}).

start_link(MountPoint) ->
    gen_server:start_link(?MODULE, MountPoint, []).

init(MountPoint) ->
    {ok, Fd} = fuse:mount(MountPoint),
    spawn_link(fun() -> 
		       lists:foreach(fun(Chan) ->
					     initialize_channel(Chan, Fd)
				     end,
				     [output_chan, input_chan])
	       end),
    {ok, #state{fd=Fd}}.

initialize_channel(Chan, Fd) ->
    {ok, _} = efuse_sup:start_child({erlang:now(),
				     {Chan, start_link, [Fd]},
				     transient,
				     brutal_kill,
				     worker,
				     [Chan]
				    }).

handle_call(_Request, _From, State) -> 
    {reply, {error, notsup}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
