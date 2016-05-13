package rank2

import scala.collection.{mutable => mu}

sealed abstract class Expr
sealed case class Variable(i: Int, name: String) extends Expr
sealed case class Imp(t1: Expr, t2: Expr) extends Expr


class AcyclicSemiUnification(n: Int) {
  type Substitution = Map[Variable, Expr]
  val varSet = Array.fill(n + 1)(mu.Set[String]())
  val rules = Array.fill(n)(mu.ArrayBuffer[(Expr, Expr)]())

  def addVariable(tau: Variable): Unit = tau match {
    case Variable(ti, tname) =>
      if (! varSet(ti)(tname)) varSet(ti) += tname
  }
  def addRule(ind: Int, tau: Expr, mu: Expr): Unit =
    rules(ind) += ((tau, mu))
  def solve: (Substitution, List[Substitution]) = throw new RuntimeException("not impl: solve")
}