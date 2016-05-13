package rank2

import common.LambdaParser
import common.LambdaTerm

object Rank2Example {
  val term1 = new LambdaParser().parse("(\\f. \\x. f (f x)) (\\y. y)")
  val term2 = new LambdaParser().parse("(\\f. \\x. f (f x)) ((\\g. \\v. g (g v)) (\\y. y))")
  val term3 = new LambdaParser().parse("(\\id. id id) (\\x. x)") // inherently needs rank-2 polymorphism
  val term4 = new LambdaParser().parse("(\\n. n n) (\\f. \\x. f (f x))") // 2^2 in Church-encoded nats
  def terms: List[LambdaTerm] = List(term1, term2, term3, term4)
}