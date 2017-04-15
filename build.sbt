organization in ThisBuild := "stanford-ppl"

scalaVersion in ThisBuild := "2.12.1"

version in ThisBuild := "1.0"

isSnapshot in ThisBuild := true

val scalatestVersion = "3.0.1"
val paradiseVersion = "2.1.0"
val virtualizedVersion = "0.2"

val commonSettings = Seq(

  libraryDependencies += "org.virtualized" %% "virtualized" % virtualizedVersion,
  libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",

  //paradise
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
)

publishArtifact := false

lazy val forge: Project = project
  .settings(commonSettings)

lazy val core = project
  .settings(commonSettings)
  .dependsOn(forge)

