ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"


lazy val root = (project in file("."))
  .settings(
    name := "aspic-flexdds",
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_3" % "2.3.0",
    libraryDependencies += "com.github.scopt" % "scopt_2.13" % "4.1.0"
  )
