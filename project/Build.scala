import sbt._
import Keys.{resolvers, _}

object ArgonBuild extends Build {
  val compilerVersion = "2.12.1"
  val scalatestVersion = "3.0.1"
  val paradiseVersion = "2.1.0" // "3.0.0-M7"
  val metaVersion = "1.6.0"

  lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := "stanford-ppl",
    scalaVersion := compilerVersion,

    publishArtifact in (Compile, packageDoc) := false,
    //libraryDependencies += "org.virtualized" %% "virtualized" % virtualizedVersion,
    libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile",

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


    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions += "-Xplugin-require:macroparadise",

    /** Macro Paradise **/
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),

    /*
    /** SCALA-META **/
    libraryDependencies += "org.scalameta" %% "scalameta" % metaVersion,
    libraryDependencies += "org.scalameta" %% "contrib" % metaVersion,

    resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),

    addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full),

    scalacOptions += "-Xplugin-require:macroparadise",
    // temporary workaround for https://github.com/scalameta/paradise/issues/10
    scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
    // temporary workaround for https://github.com/scalameta/paradise/issues/55
    sources in (Compile, doc) := Nil, // macroparadise doesn't work with scaladoc yet.
    */

    scalaSource in Compile := baseDirectory(_/ "src").value,
    scalaSource in Test := baseDirectory(_/"test").value,

    parallelExecution in Test := false,
    concurrentRestrictions in Global += Tags.limitAll(1) // we need tests to run in isolation across all projects
  )

  lazy val argonSettings = buildSettings ++ Seq(
    name := "argon",
    version := "1.0",
    isSnapshot := true,
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"
  )

  lazy val macros = Project("macros", file("macros"), settings = buildSettings)
  lazy val virtualized = Project("virtualized", file("scala-virtualized"), settings = buildSettings)

  lazy val argon = Project("argon", file("."), settings = argonSettings) dependsOn (macros, virtualized)
}


