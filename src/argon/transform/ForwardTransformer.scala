package argon.transform

import argon.core.Statements
import argon.traversal.Traversal

trait ForwardTransformer extends Transformer with Traversal { self =>
  val IR: Statements
  import IR._

  final def mirror(stm: Stm): List[Exp[_]] = {
    val id = IR.curEdgeId
    log(c"Mirror: $stm")
    stm.lhs.foreach{s =>
      if (stm.lhs.length > 1) log(c"$s")
      metadata.get(s).foreach{m => log(c" - ${m._1}: ${m._2}") }
    }

    val lhs2 = mirror(stm.lhs, stm.rhs)

    // FIXME: Hack: only mirror metadata if the symbol is new
    log(c"Result: ${str(lhs2)}")
    lhs2.foreach{
      case s2: Sym[_] =>
        if (s2.id > id) {
          val m2 = mirror(metadata.get(s2))
          metadata.set(s2, m2)
        }
      case _ =>
    }
    lhs2.foreach{s2 =>
      if (lhs2.length > 1) log(c"$s2")
      metadata.get(s2).foreach{m => log(c" - ${m._1}: ${m._2}") }
    }

    lhs2
  }

  final def mirror(lhs: List[Sym[_]], rhs: Def): List[Exp[_]] = rhs.mirrorNode(lhs, self.asInstanceOf[Tx])
  final def mirror(props: Map[Class[_],Metadata[_]]): Map[Class[_],Metadata[_]] = props.mapValues{m => mirror(m) }
  final def mirror[M<:Metadata[_]](m: M): M = m.mirror(self.asInstanceOf[Tx]).asInstanceOf[M]
}
