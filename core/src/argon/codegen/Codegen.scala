package argon.codegen

import argon.core._
import argon.{ConstantGenFailedException, GenerationFailedException}
import argon.traversal.Traversal
import java.nio.file.{Files, Paths}
import java.io.PrintWriter
import argon.emul.{FixedPoint, FloatPoint}

trait Codegen extends Traversal {
  override val recurse: RecurseOpt = Never

  val lang: String
  val ext: String
  def out: String = s"${config.genDir}${config.sep}$lang${config.sep}"
  var emitEn: Boolean = true // Hack for masking Cpp from FPGA gen, usually always true except for chisel and cpp gen
  var compressorMap = collection.mutable.HashMap[String, (String,Int)]()
  var retimeList = collection.mutable.ListBuffer[String]()
  val pipeRtMap = collection.mutable.HashMap[(String,Int), String]()

  val maxLinesPerFile = 300  // Specific hacks for chisel             
  val numTraitsPerMixer = 50 // Specific hacks for chisel

  var stream: PrintWriter = _
  var streamName = ""
  var streamTab = collection.mutable.Map[String, Int]() // Map from filename to its tab level
  var streamLines = collection.mutable.Map[String, Int]() // Map from filename number of lines it has
  var streamExtensions = collection.mutable.Map[String, List[Int]]() // Map from filename to number of extensions it has
  var streamMap = collection.mutable.Map[PrintWriter, String]() // Map from PrintWriter to its string name
  var streamMapReverse = collection.mutable.Map[String, PrintWriter]() // Map from PrintWriter to its string name
  var topLayerTraits = List[String]() // List of top layer nodes, used in multifile==4 generation
  val tabWidth: Int = 2

  def tabbed: String = " "*(tabWidth*(streamTab getOrElse (streamName, 0)))

  protected def emit(x: String, forceful: Boolean = false): Unit = { 
    if (emitEn | forceful) {
      stream.println(tabbed + x)
    } else { 
      if (config.emitDevel == 2) {Console.println(s"[ ${lang}gen-NOTE ] Emission of $x does not belong in this backend")}
    }
  } 
  protected def open(x: String, forceful: Boolean = false): Unit = {
    if (emitEn | forceful) {
      stream.println(tabbed + x); if (streamTab contains streamName) streamTab(streamName) += 1 
    } else { 
      if (config.emitDevel == 2) {Console.println(s"[ ${lang}gen-NOTE ] Emission of $x does not belong in this backend")}
    }
  }
  protected def close(x: String, forceful: Boolean = false): Unit = { 
    if (emitEn | forceful) {
      if (streamTab contains streamName) streamTab(streamName) -= 1; stream.println(tabbed + x)
    } else { 
      if (config.emitDevel == 2) {Console.println(s"[ ${lang}gen-NOTE ] Emission of $x does not belong in this backend")}
    }
  } 
  protected def closeopen(x: String, forceful: Boolean = false): Unit = { // Good for "} else {" lines
    if (emitEn | forceful) {
      if (streamTab contains streamName) streamTab(streamName) -= 1; stream.println(tabbed + x); streamTab(streamName) += 1
    } else { 
      if (config.emitDevel == 2) {Console.println(s"[ ${lang}gen-NOTE ] Emission of $x does not belong in this backend")}
    }
  } 

  protected def needsFPType(tp: Type[_]): Boolean = false
  protected def spatialNeedsFPType(tp: Type[_]): Boolean = false  // TODO: Spatial specific - should not be here!

  final protected def toggleEn(): Unit = {
    if (emitEn) {
      if (config.emitDevel == 2) {
        Console.println(s"[ ${lang}gen-NOTE ] Disabling emits")
      }
      emitEn = false
    } else {
      if (config.emitDevel == 2) {
        Console.println(s"[ ${lang}gen-NOTE ] Enabling emits")
      }
      emitEn = true      
    }
  }

  final protected def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    val saveName = streamMap getOrElse (stream,"")
    stream = out
    streamName = streamMap(out)
    try { body } finally { stream.flush(); stream = save; streamName = saveName }
  }

  final protected def newStream(name: String, exten: String = ext): PrintWriter = {
    // TODO: Assert streamMap does not contain this guy already
    val fullname = name + "." + exten
    streamTab += (fullname -> 0)
    streamLines += (name -> 0)
    streamExtensions += (name -> List(0))
    Files.createDirectories(Paths.get(out))
    val file = new PrintWriter(s"$out$name.$exten")
    streamMap += (file -> fullname)
    streamMapReverse += (fullname -> file)
    file      
  }

  final protected def newTemplate(name: String, exten: String = ext): PrintWriter = {
    // TODO: Assert streamMap does not contain this guy already
    val fullname = name + "." + exten
    streamTab += (fullname -> 0)
    Files.createDirectories(Paths.get(out))
    val file = new PrintWriter(s"$out$name.$exten")
    streamMap += (file -> fullname)
    streamMapReverse += (fullname -> file)
    file      
  }

  final protected def getStream(name: String, exten: String = ext): PrintWriter = { // Use stream if it exists, otherwise maek it exist
    // TODO: Assert streamMap does not contain this guy already
    val fullname = name + "." + exten
    if (streamMapReverse.contains(fullname)) {
      streamMapReverse(fullname)
    } else {
      newStream(name, exten)
    }
  }

  final protected def listHandle(rhs: String): String = {
    val vec = if (rhs.contains("Vec")) {
      val width_extractor = "Wire\\([ ]*Vec\\(([0-9]+)[ ]*,.*".r
      val width_extractor(vw) = rhs
      s"vec${vw}_"
    } else {""}
    if (rhs.contains("Bool()")) {
      s"${vec}b"
    } else if (rhs.contains("SRFF()")) {
      s"${vec}srff"
    } else if (rhs.contains("UInt(")) {
      val extractor = ".*UInt\\(([0-9]+).W\\).*".r
      val extractor(width) = rhs
      s"${vec}u${width}"
    } else if (rhs.contains("SInt(")) {
      val extractor = ".*SInt\\(([0-9]+).W\\).*".r
      val extractor(width) = rhs
      s"${vec}s${width}"      
    } else if (rhs.contains(" FixedPoint(")) {
      val extractor = ".*FixedPoint\\([ ]*(.*)[ ]*,[ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*\\).*".r
      val extractor(s,i,f) = rhs
      val ss = if (s.contains("rue")) "s" else "u"
      s"${vec}fp${ss}${i}_${f}"            
    } else if (rhs.contains(" FloatingPoint(")) {
      val extractor = ".*FloatingPoint\\([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*\\).*".r
      val extractor(m,e) = rhs
      s"${vec}flt${m}_${e}"            
    } else if (rhs.contains(" NBufFF(") && !rhs.contains("numWriters")) {
      val extractor = ".*NBufFF\\([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*\\).*".r
      val extractor(d,w) = rhs
      s"${vec}nbufff${d}_${w}"  
    } else if (rhs.contains(" NBufFF(") && rhs.contains("numWriters")) {
      val extractor = ".*FF\\([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*,[ ]*numWriters[ ]*=[ ]*([0-9]+)[ ]*\\).*".r
      val extractor(d,w,n) = rhs
      s"${vec}ff${d}_${w}_${n}wr"  
    } else if (rhs.contains(" templates.FF(")) {
      val extractor = ".*FF\\([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*\\).*".r
      val extractor(d,w) = rhs
      s"${vec}ff${d}_${w}"  
    } else if (rhs.contains(" multidimR(")) {
      val extractor = ".*multidimR\\([ ]*([0-9]+)[ ]*,[ ]*List\\(([0-9,]+)\\)[ ]*,[ ]*([0-9]+)[ ]*\\).*".r
      val extractor(n,dims,w) = rhs
      val d = dims.replace(" ", "").replace(",","_")
      s"${vec}mdr${n}_${d}_${w}"  
    } else if (rhs.contains(" multidimW(")) {
      val extractor = ".*multidimW\\([ ]*([0-9]+)[ ]*,[ ]*List\\(([0-9,]+)\\)[ ]*,[ ]*([0-9]+)[ ]*\\).*".r
      val extractor(n,dims,w) = rhs
      val d = dims.replace(" ", "").replace(",","_")
      s"${vec}mdw${n}_${d}_${w}"  
    } else if (rhs.contains(" multidimRegW(")) {
      val extractor = ".*multidimRegW\\([ ]*([0-9]+)[ ]*,[ ]*List\\(([0-9, ]+)\\)[ ]*,[ ]*([0-9]+)[ ]*\\).*".r
      val extractor(n,dims,w) = rhs
      val d = dims.replace(" ", "").replace(",","_")
      s"${vec}mdrw${n}_${d}_${w}"  
    } else if (rhs.contains(" Seqpipe(")) {
      val extractor = ".*Seqpipe\\([ ]*([0-9]+)[ ]*,[ ]*isFSM[ ]*=[ ]*([falsetrue]+)[ ]*,[ ]*ctrDepth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*stateWidth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*staticNiter[ ]*=[ ]*([falsetrue]+),[ ]*isReduce[ ]*=[ ]*([falsetrue]+)\\).*".r
      val extractor(stages,fsm,ctrd,stw,static,isRed) = rhs
      val f = fsm.replace("false", "f").replace("true", "t")
      val s = static.replace("false", "f").replace("true", "t")
      val ir = isRed.replace("false", "f").replace("true", "t")
      s"${vec}seq${stages}_${f}_${ctrd}_${stw}_${s}_${ir}"
    } else if (rhs.contains(" Metapipe(")) {
      val extractor = ".*Metapipe\\([ ]*([0-9]+)[ ]*,[ ]*isFSM[ ]*=[ ]*([falsetrue]+)[ ]*,[ ]*ctrDepth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*stateWidth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*staticNiter[ ]*=[ ]*([falsetrue]+),[ ]*isReduce[ ]*=[ ]*([falsetrue]+)\\).*".r
      val extractor(stages,fsm,ctrd,stw,static,isRed) = rhs
      val f = fsm.replace("false", "f").replace("true", "t")
      val s = static.replace("false", "f").replace("true", "t")
      val ir = isRed.replace("false", "f").replace("true", "t")
      s"${vec}meta${stages}_${f}_${ctrd}_${stw}_${s}_${ir}"
    } else if (rhs.contains(" Innerpipe(")) {
      val extractor = ".*Innerpipe\\([ ]*([falsetrue]+)[ ]*,[ ]*ctrDepth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*stateWidth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*staticNiter[ ]*=[ ]*([falsetrue]+),[ ]*isReduce[ ]*=[ ]*([falsetrue]+)\\).*".r
      val extractor(strm,ctrd,stw,static,isRed) = rhs
      val st = strm.replace("false", "f").replace("true", "t")
      val s = static.replace("false", "f").replace("true", "t")
      val ir = isRed.replace("false", "f").replace("true", "t")
      s"${vec}inner${st}_${ctrd}_${stw}_${s}_${ir}"
    } else if (rhs.contains(" Streaminner(")) {
      val extractor = ".*Streaminner\\([ ]*([falsetrue]+)[ ]*,[ ]*ctrDepth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*stateWidth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*staticNiter[ ]*=[ ]*([falsetrue]+),[ ]*isReduce[ ]*=[ ]*([falsetrue]+)\\).*".r
      val extractor(strm,ctrd,stw,static,isRed) = rhs
      val st = strm.replace("false", "f").replace("true", "t")
      val s = static.replace("false", "f").replace("true", "t")
      val ir = isRed.replace("false", "f").replace("true", "t")
      s"${vec}strinner${st}_${ctrd}_${stw}_${s}_${ir}"
    } else if (rhs.contains(" Parallel(")) {
      val extractor = ".*Parallel\\([ ]*([0-9]+)[ ]*,[ ]*isFSM[ ]*=[ ]*([falsetrue]+)[ ]*,[ ]*ctrDepth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*stateWidth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*staticNiter[ ]*=[ ]*([falsetrue]+),[ ]*isReduce[ ]*=[ ]*([falsetrue]+)\\).*".r
      val extractor(stages,fsm,ctrd,stw,static,isRed) = rhs
      val f = fsm.replace("false", "f").replace("true", "t")
      val s = static.replace("false", "f").replace("true", "t")
      val ir = isRed.replace("false", "f").replace("true", "t")
      s"${vec}parallel${stages}_${f}_${ctrd}_${stw}_${s}_${ir}"
    } else if (rhs.contains(" Streampipe(")) {
      val extractor = ".*Streampipe\\([ ]*([0-9]+)[ ]*,[ ]*isFSM[ ]*=[ ]*([falsetrue]+)[ ]*,[ ]*ctrDepth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*stateWidth[ ]*=[ ]*([0-9]+)[ ]*,[ ]*staticNiter[ ]*=[ ]*([falsetrue]+),[ ]*isReduce[ ]*=[ ]*([falsetrue]+)\\).*".r
      val extractor(stages,fsm,ctrd,stw,static,isRed) = rhs
      val f = fsm.replace("false", "f").replace("true", "t")
      val s = static.replace("false", "f").replace("true", "t")
      val ir = isRed.replace("false", "f").replace("true", "t")
      s"${vec}strmpp${stages}_${f}_${ctrd}_${stw}_${s}_${ir}"
    } else if (rhs.contains("_retime")) {
      "rt"
    } else {
      throw new Exception(s"Cannot compress ${rhs}!")
    }
  }

  final protected def wireMap(x: String): String = { 
    if (config.multifile == 5 | config.multifile == 6) {
      if (compressorMap.contains(x)) {
        src"${listHandle(compressorMap(x)._1)}(${compressorMap(x)._2})"
      // if (boolMap.contains(x)) {
      //   src"b(${boolMap(x)})"
      // } else if (uintMap.contains(x)) {
      //   src"u(${uintMap(x)})"
      // } else if (sintMap.contains(x)) {
      //   src"s(${sintMap(x)})"
      // } else if (fixs32_0Map.contains(x)) {
      //   src"fs32_0(${fixs32_0Map(x)})"
      // } else if (fixu32_0Map.contains(x)) {
      //   src"fu32_0(${fixu32_0Map(x)})"
      // } else if (fixs10_22Map.contains(x)) {
      //   src"fu32_0(${fixs10_22Map(x)})"
      } else {
        x
      }
    } else {
      x
    }
  }

  protected def remap(tp: Type[_]): String = tp.toString
  protected def quoteConst(c: Const[_]): String = {
    if (config.emitDevel > 0) {
      if (emitEn) { // Want to emit but can't
        Console.println(s"[ ${lang}gen-ERROR ] No quote for $c")  
      } else { // No need to emit
        Console.println(s"[ ${lang}gen-NOTE ] Quoting of $c does not belong in this backend")
      }
      
      ""
    } else {
      throw new ConstantGenFailedException(c)
    }
  }

  protected def name(s: Dyn[_]): String = s match {
    case b: Bound[_] => s"b${b.id}"
    case s: Sym[_]   => wireMap(s"x${s.id}")
      // s.tp match {
      //   // case BooleanType => wireMap(s"x${s.id}")
      //   case _ => wireMap(s"x${s.id}")
      // }
  }

  protected def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case d: Dyn[_] if config.enableNaming => wireMap(name(d))
    case b: Bound[_] => s"b${b.id}"
    case s: Sym[_] => wireMap(s"x${s.id}")
      // s.tp match {
      //   // case BooleanType => wireMap(s"x${s.id}")
      //   case _ => wireMap(s"x${s.id}")
      // }
  }

  protected def quoteOrRemap(arg: Any): String = arg match {
    case p: Seq[_] => p.map(quoteOrRemap).mkString(", ")  // By default, comma separate Seq
    case s: Set[_] => s.map(quoteOrRemap).mkString(", ")  // TODO: Is this expected? Sets are unordered..
    case e: Exp[_] => quote(e)
    case m: Type[_] => remap(m)
    case s: String => s
    case c: Int => c.toString
    case b: Boolean => b.toString
    case l: Long => l.toString
    case d: Double => d.toString
    case l: BigDecimal => l.toString
    case l: BigInt => l.toString
    case l: FloatPoint => l.toString
    case l: FixedPoint => l.toString
    case _ => throw new RuntimeException(s"Could not quote or remap $arg (${arg.getClass})")
  }

  protected def emitBlock(b: Block[_]): Unit = visitBlock(b)
  protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = {
    if (emitEn) {
      if (config.emitDevel == 0) {
        throw new GenerationFailedException(rhs)
      } else {
        Console.println(s"[ ${lang}gen-ERROR ] no backend for $lhs = $rhs in $lang")  
      } 
    } else {
      if (config.emitDevel == 2) Console.println(s"[ ${lang}gen-NOTE ] Emission of $lhs = $rhs does not belong in this backend")
    }
  }

  protected def emitFat(lhs: Seq[Sym[_]], rhs: Def): Unit = throw new GenerationFailedException(rhs)

  protected def emitFileHeader(): Unit = { }
  protected def emitFileFooter(): Unit = { }

  // Provides automatic quoting and remapping in the src string interpolator
  // emit(src"val $x = $rhs")
  implicit class CodegenHelper(sc: StringContext) {
    def src(args: Any*): String = sc.raw(args.map(quoteOrRemap): _*).stripMargin
  }

  final override protected def visit(lhs: Sym[_], rhs: Op[_]) = emitNode(lhs, rhs)
  final override protected def visitFat(lhs: Seq[Sym[_]], rhs: Def) = emitFat(lhs, rhs)
}
