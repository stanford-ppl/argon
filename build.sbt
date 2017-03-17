scalaVersion := "2.12.1"

val scalatestVersion = "3.0.1"

val paradiseVersion = "3.0.0-M7"  // check here: https://github.com/scalamacros/paradise/releases

name := "argon"

version := "1.0"

isSnapshot := true

organization := "stanford-ppl"

scalaSource in Compile <<= baseDirectory(_/ "src")

scalaSource in Test <<= baseDirectory(_/"test")

parallelExecution in Test := false

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Test, packageBin) := true

testOptions in Test += Tests.Argument("-oDF")

libraryDependencies += "org.virtualized" %% "virtualized" % "0.6"

libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"

addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

// It would be very annoying to have to import these everywhere in this project
scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions")

scalacOptions in (Compile, doc) ++= Seq(
      "-doc-root-content",
      baseDirectory.value+"/root-doc.txt",
      "-diagrams",
      "-diagrams-debug",
      //"-diagrams-dot-timeout", "20", "-diagrams-debug",
      "-doc-title", name.value
)


