rebar3_zig
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_zig, {git, "https://host/user/rebar3_zig.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_zig
    ===> Fetching rebar3_zig
    ===> Compiling rebar3_zig
    <Plugin Output>
