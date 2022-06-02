package Utils

import scala.annotation.tailrec

/**
  * Contains the function necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator:
  /**
    * Calculate the factorial of a given number
    * @param n the number to compute
    * @return n!
    */
  def factorial(n: Int): BigInt =

    // Transformation into tail recursive function
    @tailrec
    def iter(x: Int, result: BigInt): BigInt =
      if x == 0 then result
      else iter(x - 1, result * x)

    if n < 0 then throw new Exception("Parameter should not be negative") else iter(n, 1)
  end factorial

  /**
    * Calculate the combination of two given numbers
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  def calculateCombination(n: Int, k: Int): BigInt =
    if k > n then 0 else factorial(n) / (factorial(k) * factorial(n - k))
  end calculateCombination
end ClinksCalculator
