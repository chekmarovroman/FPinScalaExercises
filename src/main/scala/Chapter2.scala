import scala.io.StdIn.readInt
import scala.math.BigInt

object Chapter2 extends App{
  //Exercise 1
  @annotation.tailrec
  def fibs(n: Int, currentNumber: Int = 1, f1: BigInt = 0, f2: BigInt = 1): BigInt = {
    if (n == currentNumber)
      f2
    else if (n == 0)
      f1
    else {
      fibs(n, currentNumber+1, f2, f2+f1)
    }
  }

  //Exercise 2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (gt(as(n - 1), as(n))) loop(n + 1)
      else false
    }
    loop(1)
  }

  //Exercise 4
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))
  // a => b => f(a, b)
  // f.curried

  //Exercise 5
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b) // (f(a))(b)
  // (a, b) => f(a)(b)
  // f.uncurried

  //Exercise 6
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
  // a => f(g(a))

}
