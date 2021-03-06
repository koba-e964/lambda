package rank2

import common._

sealed abstract class LabeledLambdaTerm {
  def toString: String
  def toStringParen: String = "(" + toString + ")"
  def freeVars: Set[String] = Set()
}
/*
 * Labeled lambda abstraction. A label must be one of 1, 2 and 3.
 */
case class LabeledLambdaAbst(v: String, l: Int, e: LabeledLambdaTerm) extends LabeledLambdaTerm {
  override def toString = "\\" + l + " " + v + ". " + e.toString
  override def freeVars = e.freeVars - v
}
case class LabeledLambdaApp(e1: LabeledLambdaTerm, e2: LabeledLambdaTerm) extends LabeledLambdaTerm {
  override def toString = e1 match {
    case LabeledLambdaApp(_, _) => e1.toString + " " + e2.toStringParen
    case _ => e1.toStringParen + " " + e2.toStringParen
  }
  override def freeVars = e1.freeVars ++ e2.freeVars
}
case class LabeledLambdaVar(v: String) extends LabeledLambdaTerm {
  override def toString = v
  override def toStringParen = v
  override def freeVars = Set(v)
}

/**
 * Section 4, System \Lambda_2^{-, \ast} of [Kfoury and Wells, 1993]
 * LabelLambda adds labels to unlabeled lambda terms.
 * Given lambda terms must have distinct bound variables.
 */
object LabelLambda {
  def active(t: LambdaTerm): List[String] = t match {
    case LambdaAbst(x, e) => x :: active(e)
    case LambdaApp(e1, e2) => active(e1) match {
      case Nil => Nil
      case _ :: ls => ls
    }
    case LambdaVar(_) => Nil
  }
  def labelSub(t: LambdaTerm, xs: List[String], level: Int): LabeledLambdaTerm = t match {
    case LambdaAbst(x, e) => val newLv = if (xs.contains(x)) level else 1
      LabeledLambdaAbst(x, newLv, labelSub(e, xs, level))
    case LambdaApp(e1, e2) => LabeledLambdaApp(labelSub(e1, xs, level), labelSub(e2, active(e2), 3))
    case LambdaVar(x) => LabeledLambdaVar(x)
  }
  def label(t: LambdaTerm): LabeledLambdaTerm = labelSub(t, active(t), 2)
}

final object LabelLambdaTest {
  def main(args: Array[String]): Unit = {
    for (t <- Rank2Example.terms) {
      AlphaConverter.assertBoundVariablesAreDistinct(t)
      println(t + " ===>")
      println(LabelLambda.label(t))
    }
  }
}