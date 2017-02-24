import sbt._
import Keys._

object ArgonBuild extends Build {
  //lazy val ARGON_HOME = sys.env.get("ARGON_HOME").getOrElse(error("Please set the ARGON_HOME environment variable."))
    
  val compilerVersion = "2.12.1"
  val scalatestVersion = "3.0.1"
  val paradiseVersion = "2.1.0"  // check here: https://github.com/scalamacros/paradise/releases

  lazy val virtBuildSettings = Defaults.defaultSettings ++ Seq(

    organization := "stanford-ppl",
    scalaVersion := compilerVersion,

    publishArtifact in (Compile, packageDoc) := false,
    libraryDependencies += "org.virtualized" %% "virtualized" % "0.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2",

    retrieveManaged := true,
    scalacOptions += "-Yno-generic-signatures",

    excludeFilter in unmanagedSources := "*template-level*" || "*app-level*" || "*resources*",

    // More strict error/warning checking
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings"),
    // It would be very annoying to have to import these everywhere in this project
    scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions"),

    scalacOptions in (Compile, doc) ++= Seq(
      "-doc-root-content", 
      baseDirectory.value+"/root-doc.txt",
      "-diagrams",
      "-diagrams-debug",
      //"-diagrams-dot-timeout", "20", "-diagrams-debug",
      "-doc-title", name.value
    ),

    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
  
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test"),

    parallelExecution in Test := false,
    concurrentRestrictions in Global += Tags.limitAll(1) //we need tests to run in isolation across all projects
  )
  lazy val argonSettings = virtBuildSettings ++ Seq(
    name := "argon",
    version := "1.0",
    isSnapshot := true 
  )


  lazy val argon = Project("argon", file("."), settings = argonSettings) dependsOn (macros)
  
  lazy val macros = Project("macros", file("macros"), settings = virtBuildSettings)
}
