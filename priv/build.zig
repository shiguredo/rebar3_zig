const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();

    const version = b.version(0, 0, 0);
    const lib = b.addSharedLibrary("{{ name }}", "nif.zig", version);
    lib.setBuildMode(mode);
    if ("{{ include_path }}".len > 0) {
        lib.addIncludeDir("{{ include_path }}");
    }
    lib.linker_allow_shlib_undefined = true;
    lib.install();

    const main_tests = b.addTest("main.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
