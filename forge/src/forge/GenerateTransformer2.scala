package forge

import language.experimental.macros
import scala.reflect.macros.blackbox

/*class GenerateTransformer2[Ctx <: blackbox.Context](val c: Ctx, rngs: List[String]) {
  import c.universe._

  object RangeDecl {
    private val To = "([^\\s]*)\\s*<-\\s*([0-9]+)\\s+to\\s+([0-9]+)".r
    private val Until = "([^\\s]*)\\s*<-\\s*([0-9]+)\\s+until\\s+([0-9]+)".r
    private val ToBy = "([^\\s]*)\\s*<-\\s*([0-9]+)\\s+to\\s+([0-9]+)\\s+by\\s+([0-9]+)".r
    private val UntilBy = "([^\\s]*)\\s*<-\\s*([0-9]+)\\s+until\\s+([0-9]+)\\s+by\\s+([0-9]+)".r

    def unapply(x: String): Option[(String,Range)] = x match {
      case To(name,start,end) => Some((name,start.toInt to end.toInt))
      case Until(name,start,end) => Some((name,start.toInt until end.toInt))
      case ToBy(name,start,end,step) => Some((name,start.toInt to end.toInt by step.toInt))
      case UntilBy(name,start,end,step) => Some((name,start.toInt until end.toInt by step.toInt))
    }
  }

  private val transformer = new Tx
  private val checker = new Check
  def apply[T<:Tree](x: T): List[T] = {
    var round = 0
    var level: List[T] = List(x)
    var ranges: List[List[String]] = rngs.split(",").map(_.trim).toList
    while (ranges.exists(needsUnroll) && round < 20) {
      val (next, delay) = ranges.partition{x => RangeDecl.unapply(x).isDefined }
      val substs = next map { case RangeDecl(name,range) => name -> range }

      val nextLevel = level.flatMap{x => if (needsUnroll(x)) transformer.t(x) else List(x) }
      level = nextLevel
      round += 1
      val completedRanges =
      /*level.foreach{x =>
        c.echo(c.enclosingPosition, s"Round $round")
        c.echo(c.enclosingPosition, showRaw(x))
        c.echo(c.enclosingPosition, showCode(x))
      }*/
    }
    if (level.exists(needsUnroll)) {
      c.warning(c.enclosingPosition, "Generation unrolling did not appear to complete after 20 iterations.")
    }
    level
  }
  private def needsUnroll(x: String): Boolean = RangeDecl.unapply(x).isDefined

  private def needsUnroll(x: Tree): Boolean = checker.check(x)

  object Ranged {
    private val Numbered = "([a-zA-Z0-9_]+)\\$([^$]*)\\$([0-9]+)".r
    private val Until = "([a-zA-Z0-9_]+)\\$([^$]*)\\$([0-9]+)until([0-9]+)".r
    private val To = "([a-zA-Z0-9_]+)\\$([^$]*)\\$([0-9]+)to([0-9]+)".r

    def unapply(x: String): Option[(String,String,Range)] = x match {
      case Numbered(name,pattern,num) => Some((name,pattern,0 until num.toInt))
      case Until(name,pattern,start,end) => Some((name,pattern, start.toInt until end.toInt))
      case To(name,pattern,start,end) => Some((name,pattern,start.toInt to end.toInt))
      case _ => None
    }
    def unapply(x: Name): Option[(Name,String,Range)] = x match {
      case x: TypeName => Ranged.unapply(x)
      case x: TermName => Ranged.unapply(x)
    }
    def unapply(x: TypeName): Option[(TypeName,String,Range)] = x match {
      case TypeName(Ranged(name,pattern,range)) => Some((TypeName(name),pattern,range))
      case _ => None
    }
    def unapply(x: TermName): Option[(TermName,String,Range)] = x match {
      case TermName(Ranged(name,pattern,range)) => Some((TermName(name),pattern,range))
      case _ => None
    }
  }
  def willUnroll(x: Name): Boolean = x match {
    case TermName(name) => Ranged.unapply(name).isDefined
    case TypeName(name) => Ranged.unapply(name).isDefined
  }

  private class Check extends Traverser {
    var needsUnroll: Boolean = false
    override def traverse(tree: Tree): Unit = tree match {
      case x: ValDef if willUnroll(x.name)   => needsUnroll = true
      case x: ClassDef if willUnroll(x.name) => needsUnroll = true
      case x: DefDef if willUnroll(x.name)   => needsUnroll = true
      case x: TypeDef if willUnroll(x.name)  => needsUnroll = true
      case CaseDef(Ident(Ranged(name,p,range)), guard, body) => needsUnroll = true
      case CaseDef(Bind(Ranged(name,p,range), Typed(expr, tpt)), guard, body) => needsUnroll = true
      case CaseDef(Bind(x, Typed(expr, Ident(Ranged(name,p,range)))), guard, body) => needsUnroll = true
      case CaseDef(Bind(x, Typed(expr, AppliedTypeTree(Ident(Ranged(name,p,range)),args))), guard, body) => needsUnroll = true
      case Bind(Ranged(name,p,range), body) => needsUnroll = true
      case _ => super.traverse(tree)
    }
    def check(x: Tree): Boolean = {
      needsUnroll = false
      traverse(x)
      needsUnroll
    }
  }


  private class Tx extends Transformer {
    var subst: Map[String,Any] = Map.empty

    implicit class RangeTx(x: Range) {
      def tx[T](p: String)(func: Int => T): List[T] = x.map{i =>
        withSubst(p -> i){ func(i) }
      }.toList
    }

    def withSubst[T](rule: (String, Any))(blk: => T): T = {
      var oldSubst = subst
      subst += rule
      val result = blk
      subst = oldSubst
      result
    }

    private def checkName(orig: String, repl: String): Unit = {
      if (orig forall Character.isDigit) {
        c.error(c.enclosingPosition, s"Cannot unroll ${orig}, as this would create an illegal name $repl")
      }
    }

    def f(x: String): String = subst.foldRight(x){case ((orig,repl),cur) => cur.replaceAll(orig,repl.toString) }
    def f(x: TypeName): TypeName = x match {case TypeName(x) =>
      val name = f(x)
      checkName(x,name)
      TypeName(name)
    }
    def f(x: TermName): TermName = x match {case TermName(x) =>
      val name = f(x)
      checkName(x,name)
      TermName(name)
    }
    def f(x: Name): Name = x match { case x: TypeName => f(x); case x: TermName => f(x) }
    def f[T<:Tree](x: T): T = transform(x).asInstanceOf[T]
    def f[T<:Tree](xs: List[T]): List[T] = xs.map{x => f(x) }
    def fs[T<:Tree](xss: List[List[T]]): List[List[T]] = xss.map{xs => f(xs) }

    def t[T<:Tree](x: T): List[T] = unroll(x) /*{
      //val level = unroll(x)
      level.map{x => if (needsUnroll(x)) f(x) else x }
    }*/
    def t[T<:Tree](xs: List[T]): List[T] = xs.flatMap{x => t(x) }
    def ts[T<:Tree](xss: List[List[T]]): List[List[T]] = xss.map{xs => t(xs) }

    //ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree)
    //DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
    //ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)
    //TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree)
    //Template(parents: List[Tree], self: ValDef, body: List[Tree])
    //CaseDef(pattern: Tree, guard: Tree, body: Tree)
    //Bind(expr: Name, tp: Tree)
    //Typed(expr: Tree, tp: Tree)
    //

    def unroll[T<:Tree](tree: T): List[T] = tree match {
      case ValDef(mods, Ranged(name,p,range), tp, rhs) =>
        range.tx(p){i => ValDef(mods, f(name), f(tp), f(rhs)).asInstanceOf[T] }

      case ClassDef(mods, Ranged(name,p,range), tparams, template) =>
        range.tx(p){i =>
          ClassDef(mods, f(name), t(tparams), f(template)).asInstanceOf[T]
        }

      case DefDef(mods,Ranged(name,p,range),tparams, vparamss, returnType, body) =>
        range.tx(p){i =>
          DefDef(mods, f(name), t(tparams), ts(vparamss), f(returnType), f(body)).asInstanceOf[T]
        }

      case TypeDef(mods,Ranged(name,p,range),tparams,rhs) =>
        range.tx(p){i => TypeDef(mods, f(name), t(tparams), f(rhs)).asInstanceOf[T] }

      // case X$X$0to4 =>
      case CaseDef(Ident(Ranged(name,p,range)), guard, body) =>
        range.tx(p){i => CaseDef(f(Ident(name)), f(guard), f(body)).asInstanceOf[T] }

      // case xX$X$0to4 =>
      case CaseDef(Bind(Ranged(name,p,range), Typed(expr, tpt)), guard, body) =>
        range.tx(p){i => CaseDef(Bind(f(name), Typed(f(expr),f(tpt))), f(guard), f(body)).asInstanceOf[T] }

      // case x: TypeX$X$0to4 =>
      case CaseDef(Bind(x, Typed(expr, Ident(Ranged(name,p,range)))), guard, body) =>
        range.tx(p){i => CaseDef(Bind(f(x), Typed(f(expr),f(Ident(name)))), f(guard), f(body)).asInstanceOf[T] }

      // case x: TypeX$X$0to4[...] =>
      case CaseDef(Bind(x, Typed(expr, AppliedTypeTree(Ident(Ranged(name,p,range)),args))), guard, body) =>
        range.tx(p){i => CaseDef(Bind(f(x), Typed(f(expr), AppliedTypeTree(f(Ident(name)),t(args)))), f(guard), f(body)).asInstanceOf[T] }

      case Bind(Ranged(name,p,range),body) =>
        range.tx(p){i => Bind(f(name), f(body)).asInstanceOf[T] }

      case _ =>
        //c.info(c.enclosingPosition, s"Skipping ${showRaw(tree)}", true)
        List(f(tree))
    }

    override def transform(tree: Tree): Tree = atPos(tree.pos) { tree match {
      case ClassDef(mods,name,tpars,template)       => ClassDef(mods,f(name),t(tpars),f(template))
      case TypeDef(mods,name,tpars,rhs)             => TypeDef(mods,f(name),t(tpars),f(rhs))
      case DefDef(mods,name,tpars,vparamss,tp,body) => DefDef(mods,f(name),t(tpars),ts(vparamss),f(tp),f(body))
      case ValDef(mods,name,tp,rhs)                 => ValDef(mods,f(name), f(tp), f(rhs))
      case Template(parents,selfType,body)          => Template(f(parents),f(selfType),t(body))
      case AppliedTypeTree(tp,args)                 => AppliedTypeTree(f(tp),t(args))
      case Block(stms,ret)                          =>
        val stms2 = t(stms) ++ t(ret)
        Block(stms2.dropRight(1),f(ret))
      case Match(select,cases)                      => Match(f(select),t(cases))
      case Select(qualifier,name)                   => Select(f(qualifier),f(name))
      case Ident(TermName(x)) if subst.contains(x)  => Literal(Constant(subst(x)))
      case Ident(x: TermName)                       => Ident(f(x))
      case Ident(x: TypeName)                       => Ident(f(x))
      case Bind(name,body)                          => Bind(f(name),f(body))
      case _ => super.transform(tree)
    }}
  }
}*/
