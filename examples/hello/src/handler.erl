-module(handler).
-compile(export_all).

-include_lib("efuse/include/fuse.hrl").

mount(MountPoint) ->
    efuse:mount(MountPoint, ?MODULE, []).

% root inode
getattr(#in_header{nodeid=1}, _Cookie) ->
    {ok, 0, [#attr{ino=1, mode=?S_IFDIR bor 8#755, nlink=2}]}.

readdir(#in_header{nodeid=1}, ReadIn, _Cookie) ->
    {dirents, [
	       #dirent{ino=1, name="."},
	       #dirent{ino=1, name=".."},
	       #dirent{ino=2, name="hello"}
	      ]
    }.
    
