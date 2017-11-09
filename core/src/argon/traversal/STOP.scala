package argon.traversal
import argon.core._

case class STOP(var IR: State) extends CompilerPass {
  override val name = "STOP"
  override protected def process[S: Type](block: Block[S]): Block[S] = sys.exit(0)
}
