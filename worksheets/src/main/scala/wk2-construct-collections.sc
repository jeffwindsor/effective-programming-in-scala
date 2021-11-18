def euclideanDivision(dividend: Int, divisor: Int): (Int, Int) =
  val quotient  = dividend / divisor
  val remainder = dividend % divisor
  (quotient, remainder)

val result = euclideanDivision(10,3)
val q = result(0)
result match
  case(quotient, _) => println(quotient)
