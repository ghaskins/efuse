-module(decoder).
-export([init/2, process/3]).

-import(output_chan, [send/1]).

-include("fuse.hrl").
-include("errno.hrl").

-record(state, {handler, cookie}).

init(Handler, Cookie) ->
    #state{handler=Handler, cookie=Cookie}.

process(Header, Payload, State) ->
    OpCode = Header#in_header.opcode,
    io:format("Msg: ~p -> ~p~n", [OpCode, Payload]),
    {ok, Error, Reply} = try decode(OpCode, Header, Payload, State)
			 catch
			     error:undef ->
				 error_logger:error_msg("OpCode: ~p undefined~n", [OpCode]),
				 {ok, ?ENOSYS, []};
			     T:E ->
				 error_logger:error_msg("General request failure: H:~p P:~p E: ~p:~p~n",
							[Header, Payload, T, E]),
				 {ok, -1, []}
			 end,
    send([#out_header{error=Error, unique=Header#in_header.unique} | Reply]).

decode(?FUSE_LOOKUP, _Header, _Payload, _State) ->
    {ok, ?ENOSYS, []};
decode(?FUSE_FORGET, _Header, _Payload, _State) ->	
    {ok, ?ENOSYS, []};
decode(?FUSE_GETATTR, Header, _Payload, State) ->
    Module = State#state.handler,
    case Module:getattr(Header#in_header.nodeid, State#state.cookie) of
	{ok, Result} when is_record(Result, attr) -> 
	    {ok, 0, [Result]}
    end;
decode(?FUSE_SETATTR, _Header, _Payload, _State) ->
    {ok, ?ENOSYS, []};
decode(?FUSE_READLINK, _Header, _Payload, _State) ->
    {ok, ?ENOSYS, []};
decode(?FUSE_SYMLINK, _Header, _Payload, _State) ->
    {ok, ?ENOSYS, []};
decode(?FUSE_MKNOD, _Header, _Payload, _State) -> 
    {ok, ?ENOSYS, []};
decode(?FUSE_MKDIR, _Header, _Payload, _State) -> 
    {ok, ?ENOSYS, []};
decode(?FUSE_UNLINK, _Header, _Payload, _State) ->
    {ok, ?ENOSYS, []};
decode(?FUSE_RMDIR, _Header, _Payload, _State) -> 
    {ok, ?ENOSYS, []};
decode(?FUSE_RENAME, _Header, _Payload, _State) ->
    {ok, ?ENOSYS, []};
decode(?FUSE_LINK, _Header, _Payload, _State) ->  
    {ok, ?ENOSYS, []};
decode(?FUSE_OPEN, _Header, _Payload, _State) ->  
    {ok, ?ENOSYS, []};
decode(?FUSE_READ, _Header, _Payload, _State) ->  
    {ok, ?ENOSYS, []};
decode(?FUSE_WRITE, _Header, _Payload, _State) -> 
    {ok, ?ENOSYS, []};
decode(?FUSE_STATFS, _Header, _Payload, State) ->
    Module = State#state.handler,
    try Module:statfs(State#state.cookie) of
	{ok, Result} when is_record(Result, kstatfs) -> 
	    {ok, 0, [Result]}
    catch
	error:undef ->
	    {ok, 0, [#kstatfs{namelen=255, bsize=512}]}
    end;
decode(?FUSE_RELEASE, _Header, _Payload, _State) ->       
    {ok, ?ENOSYS, []};
decode(?FUSE_FSYNC , _Header, _Payload, _State) ->        
    {ok, ?ENOSYS, []};
decode(?FUSE_SETXATTR, _Header, _Payload, _State) ->      
    {ok, ?ENOSYS, []};
decode(?FUSE_GETXATTR, _Header, _Payload, _State) ->      
    {ok, ?ENOSYS, []};
decode(?FUSE_LISTXATTR, _Header, _Payload, _State) ->     
    {ok, ?ENOSYS, []};
decode(?FUSE_REMOVEXATTR, _Header, _Payload, _State) ->   
    {ok, ?ENOSYS, []};
decode(?FUSE_FLUSH, _Header, _Payload, _State) ->         
    {ok, ?ENOSYS, []};
decode(?FUSE_OPENDIR, _Header, _Payload, _State) ->       
    {ok, ?ENOSYS, []};
decode(?FUSE_READDIR, _Header, _Payload, _State) ->       
    {ok, ?ENOSYS, []};
decode(?FUSE_RELEASEDIR, _Header, _Payload, _State) ->    
    {ok, ?ENOSYS, []};
decode(?FUSE_FSYNCDIR, _Header, _Payload, _State) ->      
    {ok, ?ENOSYS, []};
decode(?FUSE_GETLK, _Header, _Payload, _State) ->         
    {ok, ?ENOSYS, []};
decode(?FUSE_SETLK, _Header, _Payload, _State) ->         
    {ok, ?ENOSYS, []};
decode(?FUSE_SETLKW, _Header, _Payload, _State) ->        
    {ok, ?ENOSYS, []};
decode(?FUSE_ACCESS, _Header, _Payload, _State) ->        
    {ok, ?ENOSYS, []};
decode(?FUSE_CREATE, _Header, _Payload, _State) ->        
    {ok, ?ENOSYS, []};
decode(?FUSE_INTERRUPT, _Header, _Payload, _State) ->    
    {ok, ?ENOSYS, []};
decode(?FUSE_BMAP, _Header, _Payload, _State) ->          
    {ok, ?ENOSYS, []};
decode(?FUSE_DESTROY, _Header, _Payload, _State) ->       
    {ok, ?ENOSYS, []};
decode(?FUSE_IOCTL, _Header, _Payload, _State) ->         
    {ok, ?ENOSYS, []};
decode(?FUSE_POLL, _Header, _Payload, _State) ->
    {ok, ?ENOSYS, []};
decode(?FUSE_NOTIFY_REPLY, _Header, _Payload, _State) ->  
    {ok, ?ENOSYS, []};
decode(?FUSE_BATCH_FORGET, _Header, _Payload, _State) ->  
    {ok, ?ENOSYS, []}.

