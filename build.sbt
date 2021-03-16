val dottyVersion = "3.0.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    semanticdbEnabled := true,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.3",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3-7-33b03e0",
  )
