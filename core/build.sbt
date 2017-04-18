name := "argon"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.7.0"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"
libraryDependencies += "commons-io" % "commons-io" % "2.5"

scalaSource in Compile := baseDirectory(_/ "src").value
scalaSource in Test := baseDirectory(_/"test").value

/** Scalac Options **/
scalacOptions += "-Yno-generic-signatures"
//  scalacOptions += "-Ymacro-debug-lite",
// More strict error/warning checking
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
// It would be very annoying to have to import these everywhere in this project
scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions")

// Options for auto-documentation
/*scalacOptions in (Compile, doc) ++= Seq(
  "-doc-root-content",
  baseDirectory.value+"/root-doc.txt",
  "-diagrams",
  "-diagrams-debug",
  //"-diagrams-dot-timeout", "20", "-diagrams-debug",
  "-doc-title", name.value
)*/

parallelExecution in Test := false
concurrentRestrictions in Global += Tags.limitAll(1) // we need tests to run in isolation across all projects
resourceDirectory in Compile :=  baseDirectory(_/ "resources").value
