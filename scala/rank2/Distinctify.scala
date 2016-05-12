package rank2

import common._
import scala.collection.{mutable => mu}

/**
 * Takes a lambda term, alpha-converts it and returns the resulting term.
 * All bound variables in the returned term are different.
 */
object Distinctify {
  /*
   * Generates a new variable name. The result is guaranteed not to be in used.
   */
  @annotation.tailrec
  def gen(used: mu.Set[String], x: String, n: Int): String = {
    val newName = x + n
    if (used(newName) == false) {
      used += newName
      newName
    }
    else gen(used, x, n + 1)
  }
  def actSub(env: Map[String, String], used: mu.Set[String], t: LambdaTerm): LambdaTerm = t match {
    case LambdaAbst(x, e) => val newX = gen(used, x, 0)
      LambdaAbst(newX, actSub(env + (x -> newX), used, e))
    case LambdaApp(e1, e2) => LambdaApp(actSub(env, used, e1), actSub(env, used, e2))
    case LambdaVar(x) => LambdaVar(env.getOrElse(x, x))
  }
  /**
   * Input: lambda term (possibly not closed)
   * Output: alpha-converted term, with free variables left intact, and bound variables renamed.
   */
  def act(t: LambdaTerm): LambdaTerm = actSub(Map(), mu.Set(), t)
}

object DistinctifyTest {
  def main(args: Array[String]): Unit = {
    println(Distinctify.act(new LambdaParser().parse("(\\f. \\x. f (f x)) (\\f. \\x. f (f x)) (\\x. x)")))
    println(Distinctify.act(new LambdaParser().parse("x")))
  }
}