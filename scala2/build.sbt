import org.typelevel.scalacoptions.ScalacOptions

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.17"

ThisBuild / semanticdbEnabled := true                        // For ScalaFix
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision // For ScalaFix

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-scala2",
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"                     % "2.13.0",
      "org.typelevel" %% "cats-effect"                   % "3.6.3",
      "org.typelevel" %% "mouse"                         % "1.4.0",
      "org.typelevel" %% "cats-parse"                    % "1.1.0",
      "co.fs2"        %% "fs2-core"                      % "3.12.2",
      "dev.optics"    %% "monocle-core"                  % "3.3.0",
      "dev.optics"    %% "monocle-macro"                 % "3.3.0",
      "tools.aqua"     % "z3-turnkey"                    % "4.14.1",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.7.0" % Test,
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
  "org.typelevel"       %% "typelevel-scalafix" % "0.5.0",
  "com.github.vovapolu" %% "scaluzzi"           % "0.1.23",
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

addCommandAlias("build", ";scalafmtSbt;scalafmtAll;scalafix;test")
