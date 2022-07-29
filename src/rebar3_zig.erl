-module(rebar3_zig).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar3_zig_compile:init(State0),
    {ok, State2} = rebar3_zig_test:init(State1),
    {ok, State3} = rebar3_zig_clean:init(State2),
    {ok, State3}.
