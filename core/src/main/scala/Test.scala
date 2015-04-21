import test._

object Test extends App {

  class Baz
  val baz = new Baz
  val wBaz = Witness(baz)
  type BazT = wBaz.T

}

