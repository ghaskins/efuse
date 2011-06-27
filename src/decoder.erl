-module(decoder).
-export([init/5, process/3]).

-import(output_chan, [send/2]).

-include("fuse.hrl").
-include("errno.hrl").

-record(state, {handler, cookie, opid, apiver, flags}).

init(Handler, Cookie, OPid, ApiVer, Flags) ->
    #state{handler=Handler, cookie=Cookie, opid=OPid, apiver=ApiVer, flags=Flags}.

process(Header, Payload, State) ->
    OpCode = Header#in_header.opcode,
    io:format("Msg: ~p -> ~p~n", [OpCode, Payload]),
    {ok, Error, Reply} = safe_decode(OpCode, Header, Payload, State),
    send(State#state.opid,
	 [#out_header{error=Error, unique=Header#in_header.unique} | Reply]).

safe_decode(OpCode, Header, Payload, State) ->
 try decode(OpCode, Header, Payload, State)
 catch
     throw:nohandler ->
	 error_logger:error_msg("OpCode: ~p undefined ~n", [OpCode]),
	 {ok, -?ENOSYS, []}
 end.    

decode(?FUSE_LOOKUP, Header, Payload, State) ->
    Name = binary:bin_to_list(Payload, {0, size(Payload)-1}),
    eval(lookup, [Header, Name, State#state.cookie], State);
decode(?FUSE_FORGET, _Header, _Payload, _State) ->	
    {ok, -?ENOSYS, []};
decode(?FUSE_GETATTR, Header, _Payload, State) ->
    eval(getattr, [Header, State#state.cookie], State);
decode(?FUSE_SETATTR, _Header, _Payload, _State) ->
    {ok, -?ENOSYS, []};
decode(?FUSE_READLINK, _Header, _Payload, _State) ->
    {ok, -?ENOSYS, []};
decode(?FUSE_SYMLINK, _Header, _Payload, _State) ->
    {ok, -?ENOSYS, []};
decode(?FUSE_MKNOD, _Header, _Payload, _State) -> 
    {ok, -?ENOSYS, []};
decode(?FUSE_MKDIR, _Header, _Payload, _State) -> 
    {ok, -?ENOSYS, []};
decode(?FUSE_UNLINK, _Header, _Payload, _State) ->
    {ok, -?ENOSYS, []};
decode(?FUSE_RMDIR, _Header, _Payload, _State) -> 
    {ok, -?ENOSYS, []};
decode(?FUSE_RENAME, _Header, _Payload, _State) ->
    {ok, -?ENOSYS, []};
decode(?FUSE_LINK, _Header, _Payload, _State) ->  
    {ok, -?ENOSYS, []};
decode(?FUSE_OPEN, Header, Payload, State) -> 
    OpenIn = fuse_packet:open_in(Payload),
    try eval(open, [Header, OpenIn, State#state.cookie], State)
    catch
	throw:nohandler ->
	    {ok, 0, [#open_out{flags=OpenIn#open_in.flags}]}
    end;
decode(?FUSE_READ, Header, Payload, State) ->
    ReadIn = fuse_packet:read_in(Payload),
    eval(read, [Header, ReadIn, State#state.cookie], State);
decode(?FUSE_WRITE, _Header, _Payload, _State) -> 
    {ok, -?ENOSYS, []};
decode(?FUSE_STATFS, _Header, _Payload, State) ->
    try eval(statfs, [State#state.cookie], State)
    catch
	throw:nohandler ->
	    {ok, 0, [#kstatfs{namelen=255, bsize=512}]}
    end;
decode(?FUSE_RELEASE, Header, Payload, State) ->
    ReleaseIn = fuse_packet:release_in(Payload),
    try eval(release, [Header, ReleaseIn, State#state.cookie], State)
    catch
	throw:nohandler ->
	    {ok, 0, []}
    end;
decode(?FUSE_FSYNC , _Header, _Payload, _State) ->        
    {ok, -?ENOSYS, []};
decode(?FUSE_SETXATTR, _Header, _Payload, _State) ->      
    {ok, -?ENOSYS, []};
decode(?FUSE_GETXATTR, _Header, _Payload, _State) ->      
    {ok, -?ENOSYS, []};
decode(?FUSE_LISTXATTR, _Header, _Payload, _State) ->     
    {ok, -?ENOSYS, []};
decode(?FUSE_REMOVEXATTR, _Header, _Payload, _State) ->   
    {ok, -?ENOSYS, []};
decode(?FUSE_FLUSH, _Header, _Payload, _State) ->         
    {ok, -?ENOSYS, []};
decode(?FUSE_OPENDIR, Header, Payload, State) -> 
    OpenIn = fuse_packet:open_in(Payload),
    try eval(opendir, [Header, OpenIn, State#state.cookie], State)
    catch
	throw:nohandler ->
	    {ok, 0, [#open_out{flags=OpenIn#open_in.flags}]}
    end;
decode(?FUSE_READDIR, Header, Payload, State) ->       
    ReadIn = fuse_packet:read_in(Payload),
    case eval(readdir, [Header, ReadIn, State#state.cookie], State) of
	{ok, Error, Result} -> {ok, Error, Result};
	{dirents, Dirents} ->
	    {ok, EncodedData} = dirent:encode(Dirents),
	    {ok, Data} = util:binary_part(EncodedData,
					  ReadIn#read_in.offset,
					  ReadIn#read_in.size),
	    {ok, 0, Data}
    end;
decode(?FUSE_RELEASEDIR, Header, Payload, State) -> 
    ReleaseIn = fuse_packet:release_in(Payload),
    try eval(releasedir, [Header, ReleaseIn, State#state.cookie], State)
    catch
	throw:nohandler ->
	    {ok, 0, []}
    end;
decode(?FUSE_FSYNCDIR, _Header, _Payload, _State) ->      
    {ok, -?ENOSYS, []};
decode(?FUSE_GETLK, _Header, _Payload, _State) ->         
    {ok, -?ENOSYS, []};
decode(?FUSE_SETLK, _Header, _Payload, _State) ->         
    {ok, -?ENOSYS, []};
decode(?FUSE_SETLKW, _Header, _Payload, _State) ->        
    {ok, -?ENOSYS, []};
decode(?FUSE_ACCESS, _Header, _Payload, _State) ->        
    {ok, -?ENOSYS, []};
decode(?FUSE_CREATE, _Header, _Payload, _State) ->        
    {ok, -?ENOSYS, []};
decode(?FUSE_INTERRUPT, _Header, _Payload, _State) ->    
    {ok, -?ENOSYS, []};
decode(?FUSE_BMAP, _Header, _Payload, _State) ->          
    {ok, -?ENOSYS, []};
decode(?FUSE_DESTROY, _Header, _Payload, _State) ->       
    {ok, -?ENOSYS, []};
decode(?FUSE_IOCTL, _Header, _Payload, _State) ->         
    {ok, -?ENOSYS, []};
decode(?FUSE_POLL, _Header, _Payload, _State) ->
    {ok, -?ENOSYS, []};
decode(?FUSE_NOTIFY_REPLY, _Header, _Payload, _State) ->  
    {ok, -?ENOSYS, []};
decode(?FUSE_BATCH_FORGET, _Header, _Payload, _State) ->  
    {ok, -?ENOSYS, []}.

eval(Function, Args, State) ->
    Module = State#state.handler,
    case erlang:function_exported(Module, Function, length(Args)) of
	true ->
	    erlang:apply(Module, Function, Args);
	false ->
	    throw(nohandler)
    end.
