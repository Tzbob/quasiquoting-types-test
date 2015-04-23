import test._

object Test extends App {
  val o = new Object {}
  val wO = Witness(o)
  type OT = wO.T

  val x1: OT = wO.value
  implicitly[OT =:= o.type]
  val y2: OT = o
  val z3: o.type = wO.value
}

