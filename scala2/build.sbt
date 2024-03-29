import org.typelevel.scalacoptions.ScalacOptions

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

ThisBuild / semanticdbEnabled          := true                        // For ScalaFix
ThisBuild / semanticdbVersion          := scalafixSemanticdb.revision // For ScalaFix
ThisBuild / scalafixScalaBinaryVersion := scalaBinaryVersion.value    // https://github.com/scalacenter/scalafix/issues/1658

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-scala2",
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"                     % "2.10.0",
      "org.typelevel" %% "cats-effect"                   % "3.5.3",
      "org.typelevel" %% "mouse"                         % "1.2.3",
      "org.typelevel" %% "cats-parse"                    % "0.3.9",
      "co.fs2"        %% "fs2-core"                      % "3.9.4",
      "dev.optics"    %% "monocle-core"                  % "3.2.0",
      "dev.optics"    %% "monocle-macro"                 % "3.2.0",
      "tools.aqua"     % "z3-turnkey"                    % "4.12.2.1",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test,
      "org.scalatest" %% "scalatest"                     % "3.2.18", // Not "test" on purpose as it is more convenient to keep everything together
    ),
    tpolecatExcludeOptions ++=
      Set(
        ScalacOptions.warnValueDiscard,
        ScalacOptions.warnUnusedPrivates,
        ScalacOptions.warnUnusedLocals,
        ScalacOptions.warnNumericWiden,
        ScalacOptions.warnUnusedImports,
        ScalacOptions.warnNonUnitStatement,
        ScalacOptions.warnUnusedPatVars,
      ),
  )

ThisBuild / scalafixDependencies ++= List(
  "org.typelevel"       %% "typelevel-scalafix" % "0.2.0",
  "com.github.vovapolu" %% "scaluzzi"           % "0.1.23",
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

addCommandAlias("build", ";scalafmtSbt;scalafmtAll;scalafix;test")
