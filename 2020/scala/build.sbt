val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file(""))
  .settings(
    name := "advent-of-code-2020",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-Ykind-projector",
      "-source", "3.1"
    ),
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.0.0-M4",
    ),
  )
