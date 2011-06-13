-module(fuse_packet).
-include("fuse.hrl").
-compile(export_all).

-export([in_header/1, init_in/1]).

in_header(<<Len:32/native,
	    OpCode:32/native,
	    Unique:64/native,
	    NodeId:64/native,
	    Uid:32/native,
	    Gid:32/native,
	    Pid:32/native,
	    _T/binary>>) ->
    #in_header{len=Len, opcode=OpCode, unique=Unique,
	       nodeid=NodeId, uid=Uid, gid=Gid, pid=Pid}.

init_in(<<Major:32/native,
	  Minor:32/native,
	  ReadAhead:32/native,
	  Flags:32/native>>) ->
    #init_in{major=Major, minor=Minor, max_readahead=ReadAhead, flags=Flags}.

open_in(<<Flags:32/native,
	  _/binary>>) ->
    #open_in{flags=Flags}.

read_in(<<Fh:64/native,
	  Offset:64/native,
	  Size:32/native,
	  ReadFlags:32/native,
	  LockOwner:64/native,
	  Flags:32/native,
	  _:32>>) ->
    #read_in{fh=Fh, offset=Offset, size=Size, read_flags=ReadFlags,
	     lock_owner=LockOwner, flags=Flags}.

release_in(<<Fh:64/native,
	     Flags:32/native,
	     ReleaseFlags:32/native,
	     LockOwner:64/native>>) ->
    #release_in{fh=Fh, flags=Flags, release_flags=ReleaseFlags,
		lock_owner=LockOwner}.
