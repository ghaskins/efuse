-module(handler).
-compile(export_all).

-include_lib("efuse/include/fuse.hrl").
-include_lib("efuse/include/errno.hrl").

mount(MountPoint) ->
    efuse:mount(MountPoint, ?MODULE, []).

hello_str() -> "Hello, World!".

% root inode
getattr(1) ->
    #attr{ino=1,
	  mode=?S_IFDIR bor 8#755,
	  nlink=2};
getattr(2) ->
    #attr{ino=2,
	  mode=?S_IFREG bor 8#444,
	  nlink=1,
	  size=length(hello_str())}.

getattr(#in_header{nodeid=NodeId}, _Cookie) ->
    {ok, 0, [#attr_out{attr=getattr(NodeId)}]}.

readdir(#in_header{nodeid=1}, _ReadIn, _Cookie) ->
    {dirents, [
	       #dirent{ino=1, name="."},
	       #dirent{ino=1, name=".."},
	       #dirent{ino=2, name="hello"}
	      ]
    }.

lookup(#in_header{nodeid=1}, "hello", _Cookie) ->
    {ok, 0, [#entry_out{ino=2, attr=getattr(2)}]};
lookup(_, _, _) ->
    {ok, -?ENOENT, []}.

open(#in_header{nodeid=1}, _OpenIn, _Cookie) ->
    {ok, -?EISDIR, []};
open(#in_header{nodeid=2}, OpenIn, _Cookie) ->
    if OpenIn#open_in.flags band 3 =/= ?O_RDONLY ->
	    {ok, -?EACCES, []};
       true ->
	    {ok, 0, [#open_out{flags=OpenIn#open_in.flags}]}
    end;
open(_, _, _) ->
    {ok, -?ENOENT, []}.

read(#in_header{nodeid=2}, ReadIn, _Cookie) ->
    EncodedData = binary:list_to_bin(hello_str()),
    {ok, Data} = util:binary_part([EncodedData],
				  ReadIn#read_in.offset,
				  ReadIn#read_in.size),
    {ok, 0, Data};
read(_, _, _) ->
    {ok, -?ENOENT, []}.

    

    
