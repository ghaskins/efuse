
-module(hellofuse_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(MountPoint) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MountPoint]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([MountPoint]) ->
    handler:mount(MountPoint),
    {ok, { {one_for_one, 5, 10}, []} }.

