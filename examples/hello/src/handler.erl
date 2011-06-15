-module(handler).
-compile(export_all).

-include_lib("efuse/include/fuse.hrl").
-include_lib("efuse/include/errno.hrl").

mount(MountPoint) ->
    efuse:mount(MountPoint, ?MODULE, []).

% root inode
getattr(#in_header{nodeid=1}, _Cookie) ->
    {ok, 0, [#attr_out{attr=#attr{ino=1, mode=?S_IFDIR bor 8#755, nlink=2}}]}.

readdir(#in_header{nodeid=1}, _ReadIn, _Cookie) ->
    {dirents, [
	       #dirent{ino=1, name="."},
	       #dirent{ino=1, name=".."},
	       #dirent{ino=2, name="hello"}
	      ]
    }.

lookup(#in_header{nodeid=1}, "hello", _Cookie) ->
    {ok, 0, [#entry_out{ino=2, attr=#attr{mode=?S_IFREG bor 8#444,
					  nlink=1, size=5}}]};
lookup(_, _, _) ->
    {ok, -?ENOENT, []}.

    
