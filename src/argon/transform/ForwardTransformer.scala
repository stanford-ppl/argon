package argon.transform

import argon.core.{Statements, Traversal}

trait ForwardTransformer extends Transformer with Traversal { self =>
  val IR: Statements
  import IR._

  final def mirror(stm: Stm): List[Sym] = {
    val id = IR.graph.curEdgeId
    log(c"Mirror: $stm")
    stm.lhs.foreach{s =>
      if (stm.lhs.length > 1) log(c"$s")
      metadata.get(s).foreach{m => log(c" - ${m.key}: $m") }
    }

    val lhs2 = mirror(stm.lhs, stm.rhs)

    log(c"Result: ${str(lhs2)}")
    lhs2.foreach{s2 =>
      if (s2.id > id) {
        val m2 = mirror(metadata.get(s2))
        metadata.set(s2, m2)
      }
      if (lhs2.length > 1) log(c"$s2")
      metadata.get(s2).foreach{m => log(c" - ${m.key}: $m") }
    }
    lhs2
  }

  final def mirror(lhs: List[Sym], rhs: Def): List[Sym] = rhs.mirrorNode(lhs, self.asInstanceOf[Tx])
  final def mirror(props: Set[Metadata[_]]): Set[Metadata[_]] = props.map{m => mirror(m) }
  final def mirror[M<:Metadata[_]](m: M): M = m.mirror(self.asInstanceOf[Tx]).asInstanceOf[M]
}
