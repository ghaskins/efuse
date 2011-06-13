-module(util).
-include("fuse.hrl").
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

binary_part(Data, Offset, Size) ->
    io:format("Data: ~p Offset: ~p Size: ~p~n", [Data, Offset, Size]),
    SizedData = pmap(fun(Datum) ->
			     S = size(Datum),
			     {S, Datum}
		     end,
		     Data),
    OffsetData = binary_seek(SizedData, Offset),
    NewData = [Datum || {_, Datum} <- binary_truncate(OffsetData, Size)],
    {ok, NewData}.

binary_seek(Data, Offset) ->
    binary_seek(Data, Offset, 0).

binary_seek(Data, Offset, Pos) when Offset =:= Pos ->
    Data;
binary_seek([{Size, Datum} | T], Offset, Pos) when Offset < Pos + Size -> 
    DatumOffset = Offset - Pos,
    NewSize = Size - DatumOffset,
    NewDatum = binary:part(Datum, {DatumOffset, NewSize}),
    [{NewSize, NewDatum} | T];
binary_seek([{Size, _} | T], Offset, Pos) ->
    binary_seek(T, Offset, Pos + Size);
binary_seek([], _, _) ->
    [].

binary_truncate(Data, Size) ->
    binary_truncate(Data, Size, [], 0).

binary_truncate([{DatumSize, Datum} | T], Size, NewData, Pos)
  when Size > DatumSize + Pos ->
    binary_truncate(T, Size, NewData ++ [{DatumSize, Datum}], Pos + DatumSize);
binary_truncate([{DatumSize, Datum} | T], Size, NewData, Pos) ->
    NewSize = Size - Pos,
    NewDatum = binary:part(Datum, {0, NewSize}),
    NewData ++ [{NewSize, NewDatum}];
binary_truncate([], _, NewData, _) ->
    NewData.

pmap(Fun, List) ->
    Parent = self(),
    Work = fun(Item) ->
		   try
		       Result = Fun(Item),
		       Parent ! {pmap, self(), Result}
		   catch
		       _:Error ->
			   Parent ! {pmap, self(), {error, Error}}
		   end
	   end,
    
    Pids = [spawn_link(fun() -> Work(Item) end)
	    || Item <- List],
    
    lists:map(fun(Pid) ->
		      receive
			  {pmap, Pid, {error, Error}} -> throw(Error);
			  {pmap, Pid, Result} -> Result
		      end
	      end,
	      Pids).

%-----------------------------------------------------------------------
% unit tests
%-----------------------------------------------------------------------

part_test() ->
    TestData = [<<1,2,3,4,5,6,7,8,9,10>>, <<11,12,13,14,15,16,17,18,19,20>>],
    {ok, [<<6,7,8,9,10>>, <<11,12,13,14,15>>]} = binary_part(TestData, 5, 10).
    
pmap_test() ->    
    [{1,2}, {2,4}, {3,6}, {4,8}] =
	pmap(fun(Datum) ->
		     {Datum, Datum*2}
	     end,
	     [1,2,3,4]).
