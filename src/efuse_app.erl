-module(efuse_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    RequiredArgs = [mountpoint, handler],

    lists:foldl(fun(Arg, _Acc) ->
                       case application:get_env(Arg) of
                           undefined -> throw({"Missing required arg", Arg});
                           _ -> ok
                       end
               end,
               void, RequiredArgs),

    {ok, MountPoint} = application:get_env(mountpoint),
    {ok, Handler} = application:get_env(handler),

    efuse_sup:start_link(MountPoint, Handler).

stop(_State) ->
    ok.
