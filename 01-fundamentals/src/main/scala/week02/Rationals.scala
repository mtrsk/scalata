package week02

class Rational(x: Int, y: Int) {
  require(y!=0, "Denominator must be nonzero")

  // Alternative constructor, the trival case where only the numerator is used.
  def this(x: Int) = this(x,1)

  // Euclid's GCD
  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a % b)
  private def abs(a: Int) = if(a>=0) a else -a
  private val g = abs(gcd(x,y))

  // Simplify both to avoid integer overflows
  val numer = x / g
  val denom = y / g

  // Operations
  def + (that: Rational): Rational = {
    new Rational(
      this.numer*that.denom + that.numer*this.denom,
      this.denom*that.denom
    )
  }

  // Unary negation, i.e., x => -x
  def unary_- :Rational = new Rational(-numer, denom)

  // Negation, written as an addition + unary negation.
  def - (that: Rational): Rational = this + -that

  // Relations
  def < (that: Rational): Boolean = {
    this.numer*that.denom < that.numer*this.denom
  }

  def > (that: Rational): Boolean = that < this

  def == (that: Rational): Boolean = {
    that.numer == this.numer && that.denom == this.denom
  }

  // Ordering
  def max(that: Rational): Rational = {
    if(this < that) that else this
  }

  def min(that: Rational): Rational = {
    if(this < that) this else that
  }

  // To use println
  override def toString = numer + "/" + denom
}
