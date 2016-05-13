package rank2

import common._
/**
 * Section 5, theta reduction of [Kfoury and Wells, 1993]
 */
object Theta {
  def recurse(f: LabeledLambdaTerm => LabeledLambdaTerm)(t: LabeledLambdaTerm): LabeledLambdaTerm = f(t) match {
    case LabeledLambdaAbst(x, l, e) => LabeledLambdaAbst(x, l, recurse(f)(e))
    case LabeledLambdaApp(e1, e2) => LabeledLambdaApp(recurse(f)(e1), recurse(f)(e2))
    case LabeledLambdaVar(x) => LabeledLambdaVar(x)
  }
  def theta1(t: LabeledLambdaTerm): LabeledLambdaTerm = t match {
    case LabeledLambdaApp(LabeledLambdaApp(LabeledLambdaAbst(x, 1, n), p), q)
      => LabeledLambdaApp(LabeledLambdaAbst(x, 1, LabeledLambdaApp(n, q)), p)
    case _ => t
  }
  def theta2(t: LabeledLambdaTerm): LabeledLambdaTerm = t match {
    case LabeledLambdaAbst(x, 3, LabeledLambdaApp(LabeledLambdaAbst(y, 1, n), p)) =>
      throw new RuntimeException("TODO not implemented")
    case _ => t
  }
  def theta3(t: LabeledLambdaTerm): LabeledLambdaTerm = t match {
    case LabeledLambdaApp(n, LabeledLambdaApp(LabeledLambdaAbst(x, 1, p), q)) =>
      LabeledLambdaApp(LabeledLambdaAbst(x, 1, LabeledLambdaApp(n, p)), q)
    case _ => t
  }
  def theta4(t: LabeledLambdaTerm): LabeledLambdaTerm = t match {
    case LabeledLambdaApp(LabeledLambdaAbst(x, 1, LabeledLambdaAbst(y, 2, n)), p) =>
      LabeledLambdaAbst(y, 2, LabeledLambdaApp(LabeledLambdaAbst(x, 1, n), p))
    case _ => t
  }

  /**
   * Theta-reduces the given term.
   */
  @annotation.tailrec
  def thetaNormalForm(t: LabeledLambdaTerm): LabeledLambdaTerm = {
    val newT = recurse(theta1)(recurse(theta2)(recurse(theta3)(recurse(theta4)(t))))
    if (newT == t) t
    else thetaNormalForm(newT)
  }
}

object ThetaTest {
  def main(args: Array[String]): Unit = {
    for (t <- Rank2Example.terms) {
      AlphaConverter.assertBoundVariablesAreDistinct(t)
      println(t + " ===>")
      println(Theta.thetaNormalForm(LabelLambda.label(t)))
    }
  }
}