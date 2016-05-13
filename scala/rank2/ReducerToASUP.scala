package rank2
/**
 * Reduces the typability of theta-reduced terms to acyclic semi-unification problems (ASUP).
 */
object ReducerToASUP {
  def reduce(t: LabeledLambdaTerm): AcyclicSemiUnification = {
    val asup = new AcyclicSemiUnification(10)
    asup
  }
}