lazy val root = project
  .in(file(""))
  .settings(
    name := "advent-of-code-scala3",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-Ykind-projector",
      "-source", "3.3"
    ),
    scalaVersion := "3.3.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.2",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
    ),
  )
