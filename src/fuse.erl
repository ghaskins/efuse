-module(fuse).
-export([mount/1]).
-on_load(init/0).

init() ->
    Priv = code:priv_dir(efuse),
    Path = filename:join([Priv, "efuse_nif"]),
    ok = erlang:load_nif(Path, 0).

mount(_Path) ->
    "NIF library not loaded".
