package fpinscala.laziness

import fpinscala.laziness.EmptyIsEmitted._

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] =
//    this match {
//      case Empty => None
//      case Cons(h, _) => Some(h())
//    }
    foldRight(None: Option[A])((a, _) => Some(a))

  def toList: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def takeUnfold(n: Int): Stream[A] =
    Stream.unfold((n, this)) {
      case (0, _) => None
      case (i, Cons(h, t)) =>
        val output = h()
        val nextState: (Int, Stream[A]) = (i - 1, t())
        Some((output, nextState))
      case (_, Empty) => None
    }

  @tailrec final def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(_, _) if n <= 0 => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
//    this match {
//      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
//      case _ => Empty
//    }
    foldRight(Stream.empty[A]) { (a, b) =>
      if(p(a)) Stream.cons(a, b) else Empty
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
//      case Cons(h, t) =>
//        val output = h()
//        val appliedPredicate = p(output)
//        val nextState = t()
//
//        if (appliedPredicate) Some((output, nextState))
//        else None
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
//    this match {
//      case Cons(h, t) => p(h()) && t().forAll(p)
//      case Empty => true
//    }
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B]) { (a, bs) =>
      Stream.cons(f(a), bs)
    }

  def mapUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) =>
        val output: B = f(h())
        val nextState: Stream[A] = t()
        Some((output, nextState))
      case Empty => None
    }


  def filter(pred: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (a, b) =>
      if (pred(a)) Stream.cons(a, b) else b
    }

  def append[AA >: A](as: Stream[AA]): Stream[AA] =
    foldRight(as){ (a, b) =>
      Stream.cons(a, b)
    }

  def prepend[AA >: A](as: Stream[AA]): Stream[AA] =
    // cast is safe because all As are AAs too
    as.foldRight(this.asInstanceOf[Stream[AA]]){ (a, b) =>
      Stream.cons(a, b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B]) { (a, b) =>
      f(a).append(b)
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
//      case (Empty, _) | (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) =>
        val output = f(h1(), h2())
        val nextState = (t1(), t2())

        Option((output, nextState))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (s1, s2) =>
        val output = (s1.headOption, s2.headOption)
        val nextState = (s1.drop(1), s2.drop(1))
        Some((output, nextState))
//      case (Empty, Cons(h, t)) =>
//        val output = (None, Option(h()))
//        val nextState = (Stream.empty, t())
//        Option((output, nextState))
//      case (Cons(h, t), Empty) =>
//        val output = (Option(h()), None)
//        val nextState = (t(), Stream.empty)
//        Option((output, nextState))
//      case (Cons(h1, t1), Cons(h2, t2)) =>
//        val output = (Option(h1()), Option(h2()))
//        val nextState = (t1(), t2())
//        Option((output, nextState))
//      case (Empty, Empty) => None
    }

  def startsWith[AA >: A](s: Stream[AA]): Boolean = {
    def outputAndEndStream(b: Boolean): Option[(Boolean, (Stream[A], Stream[AA]))] =
      Option((b, (Stream.empty, Stream.empty)))

    Stream.unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Option((h1() == h2(), (t1(), t2())))
      case (Cons(_, _), Empty) => outputAndEndStream(true)
      case (Empty, Cons(_, _)) => outputAndEndStream(false)
      case _ => None
    }.forAll(identity)

//    zipAll(s).foldRight(true) {
//      case ((Some(a), Some(b)), acc) => (a == b) && acc
//      case ((_, None), acc) => acc
//      case ((None, _), _) => false
//    }

//    val (s1: Stream[Option[A]], s2: Stream[Option[A]]) = Stream.unzip(zipAll(s))
//    s1.zipWith(s2.takeWhile(_.isDefined)) { (oa, ob) =>
//      (for {
//        a <- oa
//        b <- ob
//      } yield a == b).getOrElse(false)
//    }.foldRight(true)(_ && _)
  }
/**
 * For a given Stream, tails returns the Stream of suffixes of the
 * input sequence, starting with the original Stream.
 */
def tails: Stream[Stream[A]] =
  Stream.unfold((this, notYet)) {
    case (Cons(h, t), _) => Option((Cons(h, t), (t(), notYet)))
    case (Empty, NotYet) => Option((Stream.empty, (Stream.empty, yes)))
    case (_, Yes) => None
  }
//    Stream.unfold(this) {
//      case s@Cons(_, t) => Option((s, t()))
//      case Empty => None
//    }.append(Stream(Stream.empty))

//def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
//  Stream.unfold((this, z)) {
//    case (Cons(h, t), y) =>
//      val output = f(h(), y)
//      Option((output, (t(), output)))
//    case (_, _) => None
//  }
//    tails.map(_.foldRight(z)(f))

  /*
The function can't be implemented using `unfold`, since `unfold` generates
elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
The implementation is just a `foldRight` that keeps the accumulated value and
the stream of intermediate results, which we `cons` onto during each iteration.
When writing folds, it's common to have more state in the fold than is needed to
compute the result. Here, we simply extract the accumulated list once finished.
*/
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use
      // lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2

}

sealed trait EmptyIsEmitted
case object Yes extends EmptyIsEmitted
case object NotYet extends EmptyIsEmitted

object EmptyIsEmitted {
  val yes: EmptyIsEmitted = Yes
  val notYet: EmptyIsEmitted = NotYet
}
case object Empty extends Stream[Nothing]
final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def unzip[A, B](s: Stream[(A, B)]): (Stream[A], Stream[B]) =
    s match {
      case Empty => (empty, empty)
      case Cons(h, t) =>
        val (a, b) = h()
        val (t1, t2) = unzip(t())

        (cons(a, t1), cons(b, t2))
    }

  def onesPlus: Stream[Int] = Cons(() => 0, () => ones)
  def ones: Stream[Int] = constant(1) // Cons(() => 1, () => ones)
  def onesUnfold: Stream[Int] =
    unfold(1)(s => Some((s, s)))

  def constant[A](a: A): Stream[A] = {
//    cons(a, constant(a))

    // This is more efficient than `cons(a, constant(a))` since it's just
    // one object referencing itself.
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail

//    lazy val s: Stream[A] = cons(a, s)
//    s
  }

  def constantUnfold[A](a: A): Stream[A] =
    unfold(a)(s => Some((s, s)))

  def from(n: Int): Stream[Int] =
// TODO come back to this after exercise 5.13
  //    constant(n).foldRight(ones) { (a, b) =>
//      b.map(_ + a)
//    }
    cons(n, from(n + 1))

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n) { s =>
      Some((s, s + 1))
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a + b))

    go(0, 1)
  }

  val fibUnfold: Stream[Int] =
    unfold((0, 1)) { case (output, b) =>
      val nextA = b
      val nextB = output + b
      Some((output, (nextA, nextB)))
    }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  /*
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

   */
}

object Test extends App {
  println(Stream.onesUnfold.takeUnfold(10).toList)
}