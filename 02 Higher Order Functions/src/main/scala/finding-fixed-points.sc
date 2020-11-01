// Function that finds a fixed point of a function
// x is a fixed point if f(x) = x

def abs(x: Double) = if (x > 0) x else -x

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x-y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}
fixedPoint(x => 1 + x/2)(1)

// y = sqrt(x) if y * y = x ~ y = x / y, so
// sqrt(x) is a fixed point of the function y => x/y
def sqrt(x: Double) =
  // fixedPoint(y => x/y)(1) (inf loop)
  fixedPoint(y => (y + x / y) /2)(1.0)
sqrt(2)

// The stabilizing by averaging technique can be abstracted into its own function
def averageDamp(f: Double => Double)(x: Double) =
  (x + f(x)) / 2

def sqrt2(x: Double): Double =
  fixedPoint(averageDamp(y => x/y))(1.0)
sqrt2(2)