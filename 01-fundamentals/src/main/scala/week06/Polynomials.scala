package week06

package object polynomials {
  class Poly(val terms0: Map[Int, Double]) {
    val terms = terms0 withDefaultValue 0.0

    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    def + (other: Poly) = new Poly((other.terms foldLeft terms adjust)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      term + (exp -> (coeff + terms(exp)))
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted) yield coeff + "x^" + exp) mkString " + "
  }
}
