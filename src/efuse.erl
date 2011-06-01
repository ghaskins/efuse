-module(efuse).
-export([mount/2]).

mount(MountPoint, Handler) ->
    H = erlang:make_ref(),
    {ok, _} = efuse_root_sup:start_child({H,
					  {efuse_mount_sup,
					   start_link,
					   [MountPoint, Handler]},
					  transient,
					  brutal_kill,
					  supervisor,
					  [efuse_mount_sup]
				    }),
    {ok, H}.
