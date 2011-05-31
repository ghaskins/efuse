-module(output_chan).
-behavior(gen_server).

-include("fuse.hrl").

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send/1]).

-record(state, {fd}).

start_link(Fd) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Fd, []).


init(Fd) ->
    {ok, #state{fd=Fd}}.

%----------------------------------------------------------------

send([Header | Payload])
  % perform the encoding in the client context for better scaling
  when is_record(Header, out_header)->

    EncodedPayload = [ encode(Datum) || Datum <- Payload],
    Len = lists:foldl(fun(Datum, Acc) ->
			      Acc + size(Datum)
		      end,
		      0, EncodedPayload),

    EncodedHeader = encode(Header#out_header{len=Len+?OUT_HEADER_SIZE}),
    gen_server:call(?MODULE, {send, [EncodedHeader | EncodedPayload]});
send(Data) ->
    send([Data]).

%----------------------------------------------------------------

handle_call({send, Data}, _From, State) ->
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
encode(Datum) ->
    erlang:throw({"unsupported datum", Datum}). 
