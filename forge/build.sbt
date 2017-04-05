name := "forge"

scalacOptions += "-language:experimental.macros"

scalaSource in Compile := baseDirectory(_/ "src").value
scalaSource in Test := baseDirectory(_/"test").value

