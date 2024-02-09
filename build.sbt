ThisBuild / version      := "4.0.0-M4"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / organization := "io.github.reqt"

console / initialCommands := """import reqt.*"""

Global / onChangedBuildSource := ReloadOnSourceChanges

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
libraryDependencies += "org.jacop" % "jacop" % "4.9.0" 

lazy val nameOfThisBuild = "reqt-solver"

lazy val `reqt-solver` = (project in file("."))
  .settings(
    name := nameOfThisBuild,
    scalacOptions := List("-encoding", "utf8", "-Werror", "-deprecation", "-unchecked")
  )

lazy val build = taskKey[Unit]("build all the things")
build := Def.sequential(
        clean,
        Compile / compile,
        Compile / packageBin,
        Test / test,
      ).value

lazy val hello = taskKey[Unit]("Prints welcome message")

hello := println(s"""
  *** Welcome to the $nameOfThisBuild build in sbt ***

  type 'test' to run all tests
  type 'package' to build jar in target/scala-x.y.z
  type 'build' to do clean + all of the above
  type 'console' to enter the Scala REPL with reqt.* imported
  type 'hello' to see this message
""")

lazy val myStartupTransition: State => State = { s: State =>
  "hello" :: s
}

Global / onLoad := {
  // https://www.scala-sbt.org/1.0/docs/offline/Howto-Startup.html
  val old = (Global/onLoad).value
  myStartupTransition.compose(old)
}