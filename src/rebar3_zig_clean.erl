-module(rebar3_zig_clean).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, clean}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, zig},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 zig clean"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running zig clean...", []),

    Dirs = ["priv/",
            "zig_src/zig-out/",
            "zig_src/zig-cache/"],
    [ case file:del_dir_r(Dir) of
          ok ->
              rebar_api:info("Removed ~p", [Dir]);
          {error, enoent} ->
              ok;
          {error, Reason} ->
              rebar_api:abort("Could not remove directory ~p: reason=~p", [Dir, Reason])
      end || Dir <- Dirs ],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
