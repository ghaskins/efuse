-module(efuse_mount_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {erlang:now(), {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(MountPoint, Handler) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MountPoint, Handler]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([MountPoint, Handler]) ->
    {ok, Fd} = fuse:mount(MountPoint),

    {ok, { {one_for_all, 5, 10},
	   [
	    ?CHILD(input_chan, [Fd, Handler]),
	    ?CHILD(output_chan, [Fd])
	   ]} }.

