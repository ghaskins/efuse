-module(efuse_mount_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {erlang:now(), {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(MountPoint, Handler, Cookie) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MountPoint, Handler, Cookie]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([MountPoint, Handler, Cookie]) ->
    {ok, Fd} = fuse:mount(MountPoint),

    {ok, { {one_for_all, 5, 10},
	   [
	    ?CHILD(input_chan, [Fd, Handler, Cookie]),
	    ?CHILD(output_chan, [Fd])
	   ]} }.

