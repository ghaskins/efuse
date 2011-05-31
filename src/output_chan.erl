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

send(Data) when is_list(Data) ->
    gen_server:call(?MODULE, {send, Data}); 
send(Data) ->
    send([Data]).

%----------------------------------------------------------------

handle_call({send, [Header]}, _From, State) when is_record(Header, out_header) ->
    EncodedHeader = encode(Header#out_header{len=?OUT_HEADER_SIZE}),
    ok = procket:write(State#state.fd, EncodedHeader),

    {reply, ok, State};
handle_call({send, [Header | Payload]}, _From, State)
  when is_record(Header, out_header)->

    EncodedPayload = [ encode(Datum) || Datum <- Payload],
    Len = lists:foldl(fun(Datum, Acc) ->
			      Acc + size(Datum)
		      end,
		      0, EncodedPayload),

    EncodedHeader = encode(Header#out_header{len=Len+?OUT_HEADER_SIZE}),
    ok = procket:writev(State#state.fd, [EncodedHeader | EncodedPayload]),

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
