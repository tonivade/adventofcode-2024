val scala3Version = "3.6.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode-2024",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.2" % Test,
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    ) 
  )
