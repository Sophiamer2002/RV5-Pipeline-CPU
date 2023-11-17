name := "chisel_test"

version := "0.1"

scalaVersion := "2.11.12"

// resolvers ++= Seq(
//   Resolver.sonatypeRepo("snapshots"),
//   Resolver.sonatypeRepo("releases")
// )
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.4.4"
libraryDependencies +="edu.berkeley.cs" %% "chiseltest" % "0.3.3" % "test"