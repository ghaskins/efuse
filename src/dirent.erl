-module(dirent).
-include("fuse.hrl").
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(DIRENT_SIZE, 24).

namelen(Dirent) ->
    length(Dirent#dirent.name).

compute_size(Dirent) ->
    ?DIRENT_SIZE + namelen(Dirent).

align64(X) -> (X + 7) band -8. 

encode(Dirents) ->
    F = fun(#dirent{ino=Ino, name=Name, type=Type}=Dirent, Offset) ->
		BinaryName = list_to_binary(Name),
		Namelen = namelen(Dirent),
		Len = ?DIRENT_SIZE + namelen(Dirent),
		Size = align64(Len),
		Padding = binary:list_to_bin(lists:duplicate(Size-Len, 0)),
		NewOffset = Size+Offset,
		EncodedDirent = <<Ino:64/native,
				  NewOffset:64/native,
				  Namelen:32/native,
				  Type:32/native,
				  BinaryName/binary,
				  Padding/binary>>,

		if
		    size(EncodedDirent) =:= Size -> ok
		end,

		{EncodedDirent, NewOffset}
	end,
    {Data, _} = lists:mapfoldl(F, 0, Dirents),
    {ok, Data}.

%-----------------------------------------------------------------------
% unit tests
%-----------------------------------------------------------------------

encode_test() ->
    Dirents = [
	       #dirent{ino=1, name="."},
	       #dirent{ino=1, name=".."},
	       #dirent{ino=2, name="test"}
	      ],

    {ok, EncodedDirents} = encode(Dirents).


    

