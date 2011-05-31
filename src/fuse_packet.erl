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
