package fpinscala.gettingstarted

import cats.Eval

import scala.annotation.tailrec

object Fibonacci {
  def fibTailRec(n: Int): Long = {
    @tailrec
    def go(i: Int, lastTwo: (Long, Long)): Long = (i, lastTwo) match {
      case (0, (a, _)) => a
      case (1, (_, b)) => b
      case (_, (a, b)) => go(i - 1, (b, a + b))
    }

    go(n, (0, 1))
  }

  def fibEval(n: Int): Long = {
    def go: Int => Eval[(Long, Long)] = {
      case 0 => Eval.now((0, 1))
      case 1 => Eval.now((1, 1))
      case i => go(i - 1) map {
        case (a, b) => (b, a + b)
      }
    }

    go(n).value._1
  }

  def fib(i: Int): Long = fibEval(i)

}
