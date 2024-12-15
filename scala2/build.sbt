import org.typelevel.scalacoptions.ScalacOptions

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

ThisBuild / semanticdbEnabled := true                        // For ScalaFix
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision // For ScalaFix

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-scala2",
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"                     % "2.12.0",
      "org.typelevel" %% "cats-effect"                   % "3.5.7",
      "org.typelevel" %% "mouse"                         % "1.3.2",
      "org.typelevel" %% "cats-parse"                    % "1.1.0",
      "co.fs2"        %% "fs2-core"                      % "3.11.0",
      "dev.optics"    %% "monocle-core"                  % "3.3.0",
      "dev.optics"    %% "monocle-macro"                 % "3.3.0",
      "tools.aqua"     % "z3-turnkey"                    % "4.13.0.1",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.6.0" % Test,
      "org.scalatest" %% "scalatest"                     % "3.2.19", // Not "test" on purpose as it is more convenient to keep everything together
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
  "org.typelevel"       %% "typelevel-scalafix" % "0.3.1",
  "com.github.vovapolu" %% "scaluzzi"           % "0.1.23",
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

addCommandAlias("build", ";scalafmtSbt;scalafmtAll;scalafix;test")
