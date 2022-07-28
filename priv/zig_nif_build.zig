const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();

    const lib = b.addSharedLibrary("{{ name }}", "{{ name }}.zig", b.version(0, 0, 0)); // TODO: version
    lib.setBuildMode(mode);
    if ("{{ include_dir }}".len > 0) {
        lib.addIncludeDir("{{ include_dir }}");
    }
    lib.linker_allow_shlib_undefined = true;
    lib.install();

    const main_tests = b.addTest("{{ name }}.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
