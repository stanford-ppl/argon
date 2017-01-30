# Argon
Argon is an experimental framework for creating staged DSLs embbedded in Scala.
This project is based on [LMS](https://github.com/TiarkRompf/virtualization-lms-core) with some new features.

Argon currently requires the macro project scala-virtualized, although this may be merged into the project eventually.

#Prerequisites
- [Scala SBT](http://www.scala-sbt.org)
- [Java JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html)

#Installation
```bash
git clone https://github.com/stanford-ppl/scala-virtualized.git
cd scala-virtualized
sbt compile && sbt publishLocal
cd ..
git clone https://github.com/dkoeplin/argon.git
cd argon
sbt compile
```
