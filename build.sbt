val dottyVersion = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    semanticdbEnabled := true,

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % "test",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.8",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.4",
  )
