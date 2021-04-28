package fpinscala.gettingstarted

import scala.annotation.tailrec
import scala.math.abs

object GettingStarted {
  def formatResults(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def findFirst[A](as: Array[A], f: A => Boolean): Int = {
    @tailrec
    def loop(n: Int, as: Array[A]): Int = {
      if (n == as.length) -1
      else if (f(as(n))) n
      else loop(n + 1, as)
    }

    loop(0, as)
  }

  def findFirstCurried[A](as: Array[A])(f: A => Boolean): Int = findFirst(as, f)


  def compose[A,B,C](f:B=>C, g: A=>B): A=>C = a => f(g(a))
  def composeWithParam[A, B, C](f:B=>C, g: A=>B)(a: A): C = compose[A, B, C](f, g)(a)


  def main(args: Array[String]): Unit = {
    println(formatResults("Absolute value", -42, abs))
    println(findFirst(Array("Hello", "oops", "some", "world", "Blahblah", "some", "bye"), (_: String) == "some"))
    println(findFirstCurried(Array("Hello", "oops", "some", "world", "Blahblah", "some", "bye"))(_ == "some"))

  }

}
