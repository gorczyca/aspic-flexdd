//ThisBuild / version := "0.1.0-SNAPSHOT"

val version_ = "0.1.1"

ThisBuild / version := version_

ThisBuild / scalaVersion := "3.2.0"


lazy val root = (project in file("."))
  .settings(
    name := "aspic-flexdd",
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_3" % "2.3.0",
    libraryDependencies += "com.github.scopt" % "scopt_2.13" % "4.1.0",
    assembly / assemblyJarName := s"aspic-flexdd-$version_.jar"

  )
