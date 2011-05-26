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

handle_call({send, Data}, _From, State) ->
    lists:foreach(fun(Datum) ->
			  EncodedDatum = encode(Datum),
			  procket:write(State#state.fd, EncodedDatum)
		  end,
		  Data),
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
encode(#out_header{error=Error, unique=Unique})  ->
    <<16:32/native, Error:32/native, Unique:64/native>>;
encode(Datum) ->
    erlang:throw({"unsupported datum", Datum}). 
