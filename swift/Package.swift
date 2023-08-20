// swift-tools-version: 5.8
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "swift",
    platforms: [.iOS(.v16)],
    targets: [
        // Targets are the basic building blocks of a package, defining a module or a test suite.
        // Targets can depend on other targets in this package and products from dependencies.
        .executableTarget(
            name: "AdventOfCode",
            path: "Sources",
            resources: [
                .process("2017/Day25/input/test.txt"),
                .process("2017/Day25/input/real.txt")
            ]
        ),
    ]
)
