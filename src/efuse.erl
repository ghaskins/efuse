-module(efuse).
-export([mount/3]).

mount(MountPoint, Handler, Cookie) ->
    H = erlang:make_ref(),
    {ok, _} = efuse_root_sup:start_child({H,
					  {efuse_mount_sup,
					   start_link,
					   [MountPoint, Handler, Cookie]},
					  transient,
					  brutal_kill,
					  supervisor,
					  [efuse_mount_sup]
				    }),
    {ok, H}.
