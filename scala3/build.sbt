lazy val root = project
  .in(file(""))
  .settings(
    name := "advent-of-code-scala3",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-Ykind-projector",
      "-source", "3.1"
    ),
    scalaVersion := "3.2.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.4.1",
      "org.scalatest" %% "scalatest" % "3.2.14" % Test, // Did not work
    ),
  )
