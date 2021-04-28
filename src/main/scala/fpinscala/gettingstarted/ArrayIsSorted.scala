package fpinscala.gettingstarted

import scala.annotation.tailrec

object ArrayIsSorted {
  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = i match {
      case 0 => true
      case 1 => true
      case _ if ordered(as(i - 2), as(i - 1)) => go(i - 1)
      case _ => false
    }

    go(as.length)
  }
}
