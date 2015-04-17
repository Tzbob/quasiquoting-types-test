import scala.language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def impl[T](c: Context)(t: c.Expr[T]) = {
    import c.universe._
    import internal._, decorators._

    val st = t.tree match {
      case Select(a, _) =>
        singleType(a.tpe, t.tree.symbol)
      case _ => 
        sys.error("...")
    }

    println(showRaw(t.tree))
    println(showRaw(tq"$st"))
    println(showRaw(tq"${singleType(NoPrefix, t.tree.symbol)}"))
    println(showRaw(tq"${t.tree}.type"))

    c.Expr[Unit](q"")
  }

  def hello[T](t: T): Unit = macro impl[T]
}
