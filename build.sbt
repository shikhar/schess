name := "schess"

version := "0.1.0"

scalaVersion := "2.9.1"

scalacOptions := Seq("-deprecation", "-encoding", "utf8")

resolvers ++= Seq(
  "snapshots" at "http://scala-tools.org/repo-snapshots",
  "releases"  at "http://scala-tools.org/repo-releases"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.7.1" % "test"
)
