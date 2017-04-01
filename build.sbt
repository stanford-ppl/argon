organization := "ppl-stanford"

scalaVersion in ThisBuild := "2.12.1"

//publish
val suffix = ""
val versionNumber = "1.0"
version := versionNumber + suffix
isSnapshot := true

//dependencies versions
val virtualizedVersion = "0.2" + suffix
val paradiseVersion = "2.1.0"
val scalatestVersion = "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test"


//paradise
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

lazy val forgeSettings = Seq(
  libraryDependencies += "org.virtualized" %% "virtualized" % virtualizedVersion,
  //paths
  scalaSource in Compile := baseDirectory(_/ "src").value,
  scalaSource in Test := baseDirectory(_/"test").value,
  scalacOptions += "-language:experimental.macros"
)

lazy val argonSettings = Seq(
  name := "argon",
  libraryDependencies += "org.virtualized" %% "virtualized" % virtualizedVersion,
  libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2",

  //paths
  scalaSource in Compile := baseDirectory(_/ "src").value,
  scalaSource in Test := baseDirectory(_/"test").value,

  /** Scalac Options **/
  scalacOptions += "-Yno-generic-signatures",
  // More strict error/warning checking
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings"),
  // It would be very annoying to have to import these everywhere in this project
  scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions"),

  // Options for auto-documentation
  scalacOptions in (Compile, doc) ++= Seq(
    "-doc-root-content",
    baseDirectory.value+"/root-doc.txt",
    "-diagrams",
    "-diagrams-debug",
    //"-diagrams-dot-timeout", "20", "-diagrams-debug",
    "-doc-title", name.value
  ),

  parallelExecution in Test := false,
  concurrentRestrictions in Global += Tags.limitAll(1) // we need tests to run in isolation across all projects

)

lazy val forge = (project in file("forge"))
  .settings(forgeSettings)

lazy val root = (project in file("."))
  .settings(argonSettings)
  .dependsOn(forge)


