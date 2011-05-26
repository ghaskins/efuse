-module(decoder).
-export([init/1, process/3]).

-include("fuse.hrl").

-record(state, {handler}).

init(Handler) ->
    #state{handler=Handler}.

process(Header, Payload, _State) ->
    OpCode = Header#in_header.opcode,
    io:format("Msg: ~p -> ~p~n", [OpCode, Payload]),
    send(#out_header{error=-1, unique=Header#in_header.unique}).

decode(?FUSE_LOOKUP, Header, _State) ->
    [Header];
decode(?FUSE_FORGET, Header, _State) ->	
    [Header];
decode(?FUSE_GETATTR, Header, _State) ->
    [Header];
decode(?FUSE_SETATTR, Header, _State) ->
    [Header];
decode(?FUSE_READLINK, Header, _State) ->
    [Header];
decode(?FUSE_SYMLINK, Header, _State) ->
    [Header];
decode(?FUSE_MKNOD, Header, _State) -> 
    [Header];
decode(?FUSE_MKDIR, Header, _State) -> 
    [Header];
decode(?FUSE_UNLINK, Header, _State) ->
    [Header];
decode(?FUSE_RMDIR, Header, _State) -> 
    [Header];
decode(?FUSE_RENAME, Header, _State) ->
    [Header];
decode(?FUSE_LINK, Header, _State) ->  
    [Header];
decode(?FUSE_OPEN, Header, _State) ->  
    [Header];
decode(?FUSE_READ, Header, _State) ->  
    [Header];
decode(?FUSE_WRITE, Header, _State) -> 
    [Header];
decode(?FUSE_STATFS, Header, _State) ->
    [Header];
decode(?FUSE_RELEASE, Header, _State) ->       
    [Header];
decode(?FUSE_FSYNC , Header, _State) ->        
    [Header];
decode(?FUSE_SETXATTR, Header, _State) ->      
    [Header];
decode(?FUSE_GETXATTR, Header, _State) ->      
    [Header];
decode(?FUSE_LISTXATTR, Header, _State) ->     
    [Header];
decode(?FUSE_REMOVEXATTR, Header, _State) ->   
    [Header];
decode(?FUSE_FLUSH, Header, _State) ->         
    [Header];
decode(?FUSE_OPENDIR, Header, _State) ->       
    [Header];
decode(?FUSE_READDIR, Header, _State) ->       
    [Header];
decode(?FUSE_RELEASEDIR, Header, _State) ->    
    [Header];
decode(?FUSE_FSYNCDIR, Header, _State) ->      
    [Header];
decode(?FUSE_GETLK, Header, _State) ->         
    [Header];
decode(?FUSE_SETLK, Header, _State) ->         
    [Header];
decode(?FUSE_SETLKW, Header, _State) ->        
    [Header];
decode(?FUSE_ACCESS, Header, _State) ->        
    [Header];
decode(?FUSE_CREATE, Header, _State) ->        
    [Header];
decode(?FUSE_INTERRUPT, Header, _State) ->    
    [Header];
decode(?FUSE_BMAP, Header, _State) ->          
    [Header];
decode(?FUSE_DESTROY, Header, _State) ->       
    [Header];
decode(?FUSE_IOCTL, Header, _State) ->         
    [Header];
decode(?FUSE_POLL, Header, _State) ->
    [Header];
decode(?FUSE_NOTIFY_REPLY, Header, _State) ->  
    [Header];
decode(?FUSE_BATCH_FORGET, Header, _State) ->  
    [Header].

send(Data) ->
    output_chan:send(Data).
