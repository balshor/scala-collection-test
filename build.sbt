name := "scala-collection-test"

organization := "com.bizo"

version := "0.1"

scalaVersion := "2.9.0-1"

resolvers ++= Seq(
  "snapshots" at "http://scala-tools.org/repo-snapshots",
  "resleases" at "http://scala-tools.org/repo-releases"
)

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "specs" % "1.6.8" % "test"
)

testFrameworks += new TestFramework("org.specs2.runner.SpecsFramework")

