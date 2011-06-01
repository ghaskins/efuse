-module(hellofuse_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    RequiredArgs = [mountpoint],

    lists:foldl(fun(Arg, _Acc) ->
                       case application:get_env(Arg) of
                           undefined -> throw({"Missing required arg", Arg});
                           _ -> ok
                       end
               end,
               void, RequiredArgs),

    {ok, MountPoint} = application:get_env(mountpoint),

    hellofuse_sup:start_link(MountPoint).

stop(_State) ->
    ok.
