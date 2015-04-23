package test

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.Context

trait Witness extends Serializable {
  type T
  val value: T {}
}

object Witness {
  type Aux[T0] = Witness { type T = T0 }
  type Lt[Lub] = Witness { type T <: Lub }
  def apply[T](t: T): Witness.Lt[T] = macro Macros.impl[T]
}

object Macros {
  def impl[T](c: Context)(t: c.Expr[T]) = {
    import c.universe._
    import internal._, decorators._

    def mkWitness(sTpe: Type, s: Tree): Tree = {
      val name = TypeName(c.freshName())

      q"""
      {
        final class $name extends _root_.test.Witness {
          type T = $sTpe
          val value: $sTpe = $s.asInstanceOf[$sTpe]
        }
        new $name
      }
    """
    }

    def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
      val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
      val gPre = pre.asInstanceOf[global.Type]
      val gSym = sym.asInstanceOf[global.Symbol]
      global.gen.mkAttributedRef(gPre, gSym).asInstanceOf[Tree]
    }

    def prefix(tpe: Type): Type = {
      val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
      val gTpe = tpe.asInstanceOf[global.Type]
      gTpe.prefix.asInstanceOf[Type]
    }

    val tree = t.tree

    def ownerChain(sym: Symbol): List[Symbol] = {
      @tailrec
      def loop(sym: Symbol, acc: List[Symbol]): List[Symbol] =
        if(sym.owner == NoSymbol) acc
        else loop(sym.owner, sym :: acc)

      loop(sym, Nil)
    }

    val sym = tree.symbol

    def mkDependentRef(prefix: Type, path: List[Name]): (Type, Symbol) = {
      val (_, pre, sym) =
        path.foldLeft((prefix, NoType, NoSymbol)) {
          case ((pre, _, sym), nme) =>
            val sym0 = pre.member(nme)
            val pre0 = sym0.typeSignature
            (pre0, pre, sym0)
        }
      (pre, sym)
    }

    val suffix = ownerChain(sym)
    val path = suffix.tail.map(_.name.toTermName)
    val (modulePre, moduleSym) = mkDependentRef(suffix.head.typeSignature, path)

    println(showRaw(modulePre))
    println(showRaw(moduleSym))

    val pref = prefix(tree.tpe)
    val ref = mkAttributedRef(modulePre, moduleSym)
    val st = singleType(modulePre, moduleSym)
    val body = mkWitness(st, ref)

    println(body)
    println(showRaw(body))
    body
  }
}
