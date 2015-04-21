package test

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
          val value: $sTpe = $s
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

    val pref = prefix(tree.tpe)
    val ref = mkAttributedRef(pref, tree.symbol)
    val st = singleType(pref, tree.symbol)
    val body = mkWitness(st, ref)

    println(body)
    body
  }
}
