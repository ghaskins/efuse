-module(dirent).
-include("fuse.hrl").
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(DIRENT_SIZE, 25).

namelen(Dirent) ->
    length(Dirent#dirent.name).

compute_size(Dirent) ->
    ?DIRENT_SIZE + namelen(Dirent).

encode(Dirents) ->
    F = fun(#dirent{ino=Ino, name=Name, type=Type}=Dirent, Offset) ->
		BinaryName = list_to_binary(Name),
		Namelen = namelen(Dirent),
		Size = ?DIRENT_SIZE + namelen(Dirent),
		NewOffset = Size+Offset,
		EncodedDirent = <<Ino:64/native,
				  NewOffset:64/native,
				  Namelen:32/native,
				  Type:32/native,
				  BinaryName/binary,
				  0:8>>,
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


    

