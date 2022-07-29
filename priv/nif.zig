const c = @cImport(@cInclude("erl_nif.h"));
const std = @import("std");
const main = @import("main.zig");

export fn add_nif(env: ?*c.ErlNifEnv, argc: c_int, argv: [*c]const c.ERL_NIF_TERM) callconv(.C) c.ERL_NIF_TERM {
    if (argc != 2) {
        return c.enif_make_badarg(env);
    }

    var x: c_int = undefined;
    if (c.enif_get_int(env, argv[0], &x) != 1) {
        return c.enif_make_badarg(env);
    }

    var y: c_int = undefined;
    if (c.enif_get_int(env, argv[1], &y) != 1) {
        return c.enif_make_badarg(env);
    }

    const ret = main.add(x, y);
    return c.enif_make_int(env, ret);
}

export var nif_funcs = [_]c.ErlNifFunc{.{
    .name = "add",
    .arity = 2,
    .fptr = add_nif,
    .flags = 0,
}};

const ENTRY: c.ErlNifEntry = .{
    .major = c.ERL_NIF_MAJOR_VERSION,
    .minor = c.ERL_NIF_MINOR_VERSION,
    .name = "{{ name }}",
    .num_of_funcs = @intCast(c_int, nif_funcs.len),
    .funcs = &(nif_funcs[0]),
    .load = null,
    .reload = null,
    .upgrade = null,
    .unload = null,
    .vm_variant = c.ERL_NIF_VM_VARIANT,
    .options = 1,
    .sizeof_ErlNifResourceTypeInit = @sizeOf(c.ErlNifResourceTypeInit),
    .min_erts = c.ERL_NIF_MIN_ERTS_VERSION,
};

export fn nif_init() callconv(.C) *const c.ErlNifEntry {
    return &ENTRY;
}
