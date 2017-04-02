scalaVersion in ThisBuild := "2.12.1"

//publish
val suffix = ""
val versionNumber = "1.0"

//dependencies versions
val virtualizedVersion = "0.2" + suffix
val paradiseVersion = "2.1.0"
val scalatestVersion = "3.0.1"


lazy val commonSettings = Seq(
  organization := "ppl-stanford",
  version := versionNumber + suffix,
  isSnapshot := true,

  libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",

  //paradise
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),

  //paths
  scalaSource in Compile := baseDirectory(_/ "src").value,
  scalaSource in Test := baseDirectory(_/"test").value  

)

lazy val forgeSettings = commonSettings ++ Seq(

  libraryDependencies += "org.virtualized" %% "virtualized" % virtualizedVersion,

  scalacOptions += "-language:experimental.macros"
)

lazy val argonSettings = commonSettings ++ Seq(
  name := "argon",
  libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2",


  /** Scalac Options **/
  scalacOptions += "-Yno-generic-signatures",
  //  scalacOptions += "-Ymacro-debug-lite",
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

lazy val forge = (project)
  .settings(forgeSettings)

lazy val root = (project in file("core"))
  .settings(argonSettings)
  .dependsOn(forge)


