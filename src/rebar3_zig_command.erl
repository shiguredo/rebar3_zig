-module(rebar3_zig_command).

-export([execute/2]).


-spec execute(rebar_state:t(), [string()]) -> ok.
execute(_State, Args) ->
    Path = command_path(),
    Port = erlang:open_port({spawn_executable, Path},
                            [{args, Args},
                             {cd, "zig_src/"},
                             exit_status]),
    execute_output(Port).


-spec command_path() -> string().
command_path() ->
    case os:find_executable("zig") of
        false ->
            rebar_api:abort("No such command: zig", []);
        Path ->
            Path
    end.


-spec execute_output(port()) -> ok.
execute_output(Port) ->
    receive
        {Port, {data, Data}} ->
            io:format("~s", [Data]),
            execute_output(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Status}} ->
            rebar_api:abort("zig command failed: status=~p", [Status]);
        {Port, Message} ->
            rebar_api:abort("Received an unexpected message: ~p", [Message])
    after
        60000 ->
            rebar_api:abort("Timeout", [])
    end.
