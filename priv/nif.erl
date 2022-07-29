-module({{ name }}).

-export([add/2]).

-on_load(init/0).


init() ->
    case code:priv_dir({{ name }}) of
        {error, bad_name} ->
            ok = erlang:load_nif("priv/lib{{ name }}", 0);
        Path ->
            ok = erlang:load_nif(filename:join(Path, "lib{{ name }}"), 0)
    end.


add(_X, _Y) ->
    exit(nif_library_not_loaded).
