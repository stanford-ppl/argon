# Argon
Argon is an experimental framework for creating Staged DSLs embbedded in Scala.
This project is based on [LMS](https://github.com/TiarkRompf/virtualization-lms-core) with some new features.

Argon currently requires the macro project scala-virtualized, although this may be merged into the project eventually.

#Prerequisites
- [Scala SBT](http://www.scala-sbt.org)
- [Java JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html)

#Installation
```bash
git clone https://github.com/stanford-ppl/scala-virtualized.git
git clone https://github.com/stanford-ppl/argon.git

cd scala-virtualized
git checkout argon
sbt publishLocal
cd ..

cd argon
sbt compile
```
