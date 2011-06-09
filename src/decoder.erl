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
     error:undef ->
	 error_logger:error_msg("OpCode: ~p undefined~n",
				[OpCode]),
	 {ok, -?ENOSYS, []};
     T:E ->
	 error_logger:error_msg("General request failure: H:~p P:~p E: ~p:~p~n",
				[Header, Payload, T, E]),
	 {ok, -1, []}
 end.    

decode(?FUSE_LOOKUP, _Header, _Payload, _State) ->
    {ok, -?ENOSYS, []};
decode(?FUSE_FORGET, _Header, _Payload, _State) ->	
    {ok, -?ENOSYS, []};
decode(?FUSE_GETATTR, Header, _Payload, State) ->
    Module = State#state.handler,
    Module:getattr(Header#in_header.nodeid, State#state.cookie);
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
decode(?FUSE_OPEN, _Header, _Payload, _State) ->  
    {ok, -?ENOSYS, []};
decode(?FUSE_READ, _Header, _Payload, _State) ->  
    {ok, -?ENOSYS, []};
decode(?FUSE_WRITE, _Header, _Payload, _State) -> 
    {ok, -?ENOSYS, []};
decode(?FUSE_STATFS, _Header, _Payload, State) ->
    Module = State#state.handler,
    try Module:statfs(State#state.cookie)
    catch
	error:undef ->
	    {ok, 0, [#kstatfs{namelen=255, bsize=512}]}
    end;
decode(?FUSE_RELEASE, _Header, _Payload, _State) ->       
    {ok, -?ENOSYS, []};
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
decode(?FUSE_OPENDIR, _Header, <<Flags:32/native, _/binary>>, State) -> 
    Module = State#state.handler,
    try Module:opendir(Flags, State#state.cookie)
    catch
	error:undef ->
	    {ok, 0, [#open_out{flags=Flags}]}
    end;
decode(?FUSE_READDIR, _Header, _Payload, _State) ->       
    {ok, -?ENOSYS, []};
decode(?FUSE_RELEASEDIR, _Header, _Payload, _State) ->    
    {ok, -?ENOSYS, []};
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

