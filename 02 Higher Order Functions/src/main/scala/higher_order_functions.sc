def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f)(a + 1, b)
sum(x => x * x * x)(2, 3)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a+1, b)
product(x => x * x)(3,4)

def factorial(n: Int) =
  product(x => x)(1, n)
factorial(5)

// Function that generalizes both sum and product
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

def sum2(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x + y, 0)(a, b)

sum2(x => x * x * x)(2,3)

def product2(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

product2(x => x * x)(3, 4)
