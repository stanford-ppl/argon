import sbt._
import Keys.{resolvers, _}

object ArgonBuild extends Build {
  val compilerVersion = "2.12.1"
  val scalatestVersion = "3.0.1"
  val paradiseVersion = "3.0.0-M7"
  val metaVersion = "1.6.0"

  lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    scalaVersion := compilerVersion,

    /** Resolvers **/
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),

    /** Library Dependencies **/
    libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2",
    //libraryDependencies += "org.scalameta" %% "scalameta" % metaVersion,
    //libraryDependencies += "org.scalameta" %% "contrib" % metaVersion,


    /** Scalac Options **/
    scalacOptions += "-Yno-generic-signatures",
    // More strict error/warning checking
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings"),
    // It would be very annoying to have to import these everywhere in this project
    scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions", "-language:experimental.macros"),
    // Require macro-paradise
    scalacOptions += "-Xplugin-require:macroparadise",
    // Temporary workaround for https://github.com/scalameta/paradise/issues/10
    scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.

    // Options for auto-documentation
    scalacOptions in (Compile, doc) ++= Seq(
      "-doc-root-content",
      baseDirectory.value+"/root-doc.txt",
      "-diagrams",
      "-diagrams-debug",
      //"-diagrams-dot-timeout", "20", "-diagrams-debug",
      "-doc-title", name.value
    ),
    // Temporary workaround for https://github.com/scalameta/paradise/issues/55
    sources in (Compile, doc) := Nil, // macroparadise doesn't work with scaladoc yet.


    /** Compiler Plugins **/
    //addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
    addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full),


    /** Paths **/
    scalaSource in Compile := baseDirectory(_/ "src").value,
    scalaSource in Test := baseDirectory(_/"test").value,


    /** Path Exclusions **/
    excludeFilter in unmanagedSources := "*template-level*" || "*app-level*" || "*resources*",


    /** Other **/
    retrieveManaged := true,
    publishArtifact in (Compile, packageDoc) := false,
    parallelExecution in Test := false,
    concurrentRestrictions in Global += Tags.limitAll(1) // we need tests to run in isolation across all projects
  )

  lazy val forgeSettings = buildSettings ++ Seq(
    name := "forge",
    organization := "stanford-ppl",
    version := "1.0",
    isSnapshot := true
  )

  lazy val forge = Project("forge", file("."), settings = forgeSettings)
}


