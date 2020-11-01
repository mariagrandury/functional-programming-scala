class Rational(x: Int, y: Int) {
  // Test performed when the class is initialized
  require(y != 0, "denominator must be nonzero")

  // Create a second constructor
  def this(x: Int) = this(x, 1) // new Rational(2) = 2/1

  // We use gcd to simplify the rational number
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  def numer = x / g
  def denom = y / g

  override def toString =
    numer + "/" + denom

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer,denom)

  def - (that: Rational) = this + -that

  def < (that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this < that) that else this
}

val x = new Rational(1,2)
x.numer
x.denom

val y = new Rational(2,3)
x + y

-x
x - y

x < y
x max y


