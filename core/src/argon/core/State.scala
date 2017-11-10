package argon.core

import argon.graphs.Graph
import argon.util.NullOutputStream

import java.io.PrintStream
import scala.collection.mutable

/**
  * Thread-safe compiler state
  */
class State {
  /** Compiler configuration **/
  var config: Config = new Config()

  /** The IR Graph **/
  val graph: Graph[Dyn[_],Def] = new Graph[Dyn[_],Def]()(this)

  /** List of effectful statements in the current scope **/
  var context: List[Sym[_]] = _
  final def checkContext(): Unit = if (context == null) throw new argon.UninitializedEffectContextException()(this)

  /** Effects for all statements staged in the current scope **/
  var blockEffects: Effects = Effects()

  /** Definition cache used for CSE **/
  var defCache: Map[Def, Seq[Sym[_]]] = Map.empty

  /** Alias caches **/
  val shallowAliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]
  val deepAliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]
  val aliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]

  /** Scope cache - used to make scheduling faster **/
  val scopeCache = mutable.HashMap[Block[_],Seq[Stm]]()


  /** Basic blocks - skip Graph scheduling **/
  var useBasicBlocks = false
  var currentBlock: List[Stm] = Nil
  val basicBlocks = mutable.HashMap[Block[_],Seq[Stm]]()


  /** Graph Metadata **/
  val metadata: IRMetadata = new IRMetadata
  val globaldata: GlobalMetadata = new GlobalMetadata

  /** The number of the current compiler pass **/
  var pass: Int = 1

  def paddedPass: String = { val p = pass.toString; "0"*(4-p.length) + p }
  def paddedPass(pass: Int): String = { val p = pass.toString; "0"*(4-p.length) + p }

  /** The current stream being used for logging **/
  var logstream: PrintStream = new PrintStream(new NullOutputStream)

  /** The number of user errors encountered so far **/
  private var _errors: Int = 0
  def errors: Int = _errors
  def hadErrors: Boolean = errors > 0
  def logError(): Unit = { _errors += 1 }
  def resetErrors(): Unit = { _errors = 0 }

  private var _bug: Boolean = false
  def hadBug: Boolean = _bug
  def logBug(): Unit = { _bug = true }
  def resetBug(): Unit = { _bug = false }

  /** The number of user warnings encountered so far **/
  private var _warnings: Int = 0
  def warnings: Int = _warnings
  def hadWarnings: Boolean = warnings > 0
  def logWarning(): Unit = { _warnings += 1 }

  /** Number of parameters used **/
  var nParams = 0
  def nextParamId(): Int = {nParams -= 1; nParams}

  def reset(): Unit = {
    graph.reset()
    metadata.reset()
    globaldata.reset()
    context = null
    defCache = Map.empty
    shallowAliasCache.clear()
    deepAliasCache.clear()
    aliasCache.clear()
    scopeCache.clear()
    pass = 1
    logstream = new PrintStream(new NullOutputStream)
    _errors = 0
    _warnings = 0
    nParams = 0

    useBasicBlocks = false
    basicBlocks.clear()
  }
  def copyTo(that: State): State = {
    that.reset()
    this.graph.copyTo(that.graph)
    this.metadata.copyTo(that.metadata)
    this.globaldata.copyTo(that.globaldata)
    that.context = this.context
    that.defCache = this.defCache
    that.shallowAliasCache ++= this.shallowAliasCache
    that.deepAliasCache ++= this.deepAliasCache
    that.aliasCache ++= this.aliasCache
    that.scopeCache ++= this.scopeCache
    that.pass = this.pass
    that.logstream = this.logstream
    that._errors = this._errors
    that._warnings = this._warnings
    that.nParams = this.nParams

    that.basicBlocks ++= this.basicBlocks
    that.useBasicBlocks = this.useBasicBlocks
    that
  }


}
