-module(output_chan).
-behavior(gen_server).

-include("fuse.hrl").

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send/2, set_compat/3]).

-record(state, {fd, apiver={?API_MAJOR, ?API_MINOR}, flags=0}).

start_link(Fd) ->
    gen_server:start_link(?MODULE, [Fd], []).

init([Fd]) ->
    {ok, #state{fd=Fd}}.

%----------------------------------------------------------------

send(Pid, [Header | Payload]=Data)
  % perform the encoding in the client context for better scaling
  when is_record(Header, out_header)->

    EncodedPayload = [ encode(Datum) || Datum <- Payload],
    Len = lists:foldl(fun(Datum, Acc) ->
			      Acc + size(Datum)
		      end,
		      0, EncodedPayload),

    EncodedHeader = encode(Header#out_header{len=Len+?OUT_HEADER_SIZE}),
    gen_server:call(Pid, {send, [EncodedHeader | EncodedPayload]});
send(Pid, Data) ->
    send(Pid, [Data]).

set_compat(Pid, ApiVer, Flags) ->
    gen_server:call(Pid, {set_compat, ApiVer, Flags}).

%----------------------------------------------------------------

handle_call({set_compat, ApiVer, Flags}, _From, State) ->
    {reply, ok, State#state{apiver=ApiVer, flags=Flags}};
handle_call({send, Data}, _From, State) ->
    io:format("SEND: ~p~n", [Data]),
    ok = procket:writev(State#state.fd, Data),
    {reply, ok, State};
handle_call(_Request, _From, State) -> 
    {reply, {error, notsup}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%----------------------------------------------------------------

encode(Datum) when is_binary(Datum) ->
    Datum;
encode(#out_header{len=Len, error=Error, unique=Unique})  ->
    <<Len:32/native,
      Error:32/native,
      Unique:64/native>>;
encode(#init_out{major=Major, minor=Minor, max_readahead=ReadAhead,
		 flags=Flags, max_background=MaxBackground,
		 congestion_threshold=Congestion, max_write=MaxWrite}) ->
    <<Major:32/native,
      Minor:32/native,
      ReadAhead:32/native,
      Flags:32/native,
      MaxBackground:16/native,
      Congestion:16/native,
      MaxWrite:32/native>>;
encode(#kstatfs{blocks=Blocks, bfree=Bfree, bavail=Bavail,
		files=Files, ffree=Ffree, bsize=Bsize,
		namelen=Namelen, frsize=Frsize}) ->
    <<Blocks:64/native,
      Bfree:64/native,
      Bavail:64/native,
      Files:64/native,
      Ffree:64/native,
      Bsize:32/native,
      Namelen:32/native,
      Frsize:32/native,
      0:224>>;
encode(#attr_out{timeout=Timeout, attr=Attr}) ->

    BinaryAttr = encode(Attr),

    <<Timeout:64/native,
      0:32, % unused nsec field
      0:32, % "dummy" padding
      BinaryAttr/binary>>;
encode(#attr{ino=Ino, size=Size, blocks=Blocks,
	     atime=Atime, mtime=Mtime, ctime=Ctime,
	     atimensec=Atimensec, mtimensec=Mtimensec, ctimensec=Ctimensec,
	     mode=Mode, nlink=Nlink,
	     uid=Uid, gid=Gid, rdev=Rdev, blksize=Blksize}) ->
    <<Ino:64/native,
      Size:64/native,
      Blocks:64/native,
      Atime:64/native,
      Mtime:64/native,
      Ctime:64/native,
      Atimensec:32/native,
      Mtimensec:32/native,
      Ctimensec:32/native,
      Mode:32/native,
      Nlink:32/native,
      Uid:32/native,
      Gid:32/native,
      Rdev:32/native,
      Blksize:32/native,
      0:32>>; %padding
encode(#entry_out{ino=Ino, generation=Generation,
		  attr_timeout=AttrTimeout, entry_timeout=EntryTimeout,
		  attr=Attr}) ->

    BinaryAttr = encode(Attr#attr{ino=Ino}),

    <<Ino:64/native,
      Generation:64/native,
      EntryTimeout:64/native,
      AttrTimeout:64/native,
      0:64,  % unused nsec timeouts
      BinaryAttr/binary>>;
encode(#open_out{fh=Fh, flags=Flags}) ->
    <<Fh:64/native, Flags:32/native, 0:32/native>>;
encode(Datum) ->
    erlang:throw({"unsupported datum", Datum}). 
