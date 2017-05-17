package argon

import java.io.PrintStream

import argon.core.UninitializedEffectContextException
import argon.graphs.Graph
import argon.util.NullOutputStream

import scala.collection.mutable

/**
  * Threadsafe compiler state
  */
class State {
  /** The IR Graph **/
  val graph: Graph[Dyn[_],Def] = new Graph[Dyn[_],Def]

  /** List of effectful statements in the current scope **/
  var context: List[Sym[_]] = _
  final def checkContext(): Unit = if (context == null) throw new UninitializedEffectContextException()

  /** Definition cache used for CSE **/
  var defCache: Map[Def, Seq[Sym[_]]] = Map.empty

  /** Alias caches **/
  val shallowAliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]
  val deepAliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]
  val aliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]

  /** Scope cache - used to make scheduling faster **/
  val scopeCache = mutable.HashMap[Block[_],Seq[Stm]]()

  /** Graph Metadata **/
  val metadata: IRMetadata = new IRMetadata

  /** The number of the current compiler pass **/
  var pass: Int = 1

  def paddedPass: String = { val p = pass.toString; "0"*(4-p.length) + p }

  /** The current stream being used for logging **/
  var logstream: PrintStream = new PrintStream(new NullOutputStream)

  /** The number of user errors encountered so far **/
  private var _errors: Int = 0
  def errors: Int = _errors
  def hadErrors: Boolean = errors > 0
  def logError(): Unit = { _errors += 1 }

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
    context = null
    defCache = Map.empty
    shallowAliasCache.clear()
    deepAliasCache.clear()
    aliasCache.clear()
    scopeCache.clear()
    metadata.reset()
    pass = 1
    logstream = new PrintStream(new NullOutputStream)
    _errors = 0
    _warnings = 0
  }
}
