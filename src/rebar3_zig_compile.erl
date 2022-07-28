-module(rebar3_zig_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

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
            {example, "rebar3 zig compile"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->

    %% Build.
    rebar_api:info("Running zig compile...", []),
    ok = rebar3_zig_command:execute(State, ["build"]),

    %% Copy artifacts.
    rebar_api:info("Copying artifacts...", []),
    {ok, Files} = file:list_dir("zig_src/zig-out/lib/"),
    ok = filelib:ensure_dir("priv/"),
    [ begin
          Src = "zig_src/zig-out/lib/" ++ File,
          Dst = "priv/" ++ File,
          rebar_api:info("Copied: ~p => ~p", [Src, Dst]),
          {ok, _} = file:copy(Src, Dst),
          rebar_api:info("Copied: ~p => ~p", [Src, Dst])
      end || File <- Files,
             re:run(File, "^lib.*[.](so|dylib|dll)$") =/= nomatch],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
