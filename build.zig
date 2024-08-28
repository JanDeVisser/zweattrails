const std = @import("std");

pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});
    b.reference_trace = 10;

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const profile = b.addExecutable(.{
        .name = "profile",
        .root_source_file = b.path("src/profile.zig"),
        .target = target,
        .optimize = optimize,
    });

    // b.installArtifact(profile);

    const gen_profile_cmd = b.addRunArtifact(profile);
    const run_profile_step = b.step("profile", "Parse the FIT profile");
    run_profile_step.dependOn(&gen_profile_cmd.step);

    const zweattrails = b.addExecutable(.{
        .name = "zweattrails",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    zweattrails.step.dependOn(&gen_profile_cmd.step);
    zweattrails.linkSystemLibrary("raylib");
    zweattrails.root_module.addImport("pg", b.dependency("pg", .{}).module("pg"));
    zweattrails.linkLibC();
    b.installArtifact(zweattrails);

    const run_cmd = b.addRunArtifact(zweattrails);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
