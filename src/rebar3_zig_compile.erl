-module(rebar3_zig_compile).

-include_lib("kernel/include/file.hrl").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

-define(ZIG_LIB_OUT_DIR, "zig_src/zig-out/lib").

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
    %% Prepare.
    {ok, Cwd} = file:get_cwd(),
    ok = filelib:ensure_dir("priv/"),

    %% TODO: log messages
    case file:read_link_info(?ZIG_LIB_OUT_DIR) of
        {error, enoent} ->
            ok = filelib:ensure_dir(?ZIG_LIB_OUT_DIR),
            ok = file:make_symlink(Cwd ++ "/priv/", ?ZIG_LIB_OUT_DIR),
            ok;
        {error, Reason} ->
            rebar_api:abort("`file:read_link_info(\"~p\")` failure: reason=~p", [?ZIG_LIB_OUT_DIR, Reason]);
        {ok, #file_info{type = symlink}} ->
            ok;
        {ok, _} ->
            ok = file:del_dir_r(?ZIG_LIB_OUT_DIR),
            ok = file:make_symlink(Cwd ++ "/priv/", ?ZIG_LIB_OUT_DIR),
            ok
    end,

    %% Build.
    rebar_api:info("Running zig compile...", []),
    ok = rebar3_zig_command:execute(State, ["build"]),


    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
