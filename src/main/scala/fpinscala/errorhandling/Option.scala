package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] =
    map(f).flatMap(b => if(b) this else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def apply[A](a: A): Option[A] =
    if(null == a) None else Some(a)

  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- oa
      b <- ob
    } yield f(a, b)

  def sequence[Z](a: List[Option[Z]]): Option[List[Z]] =
    traverse(a)(identity)

  def traverse[A, B](a: List[A])
                    (f: A => Option[B]): Option[List[B]] =
//    sequence(a.map(f))

    a.foldRight(Option(List.empty[B])) { (h, t) =>
      map2(f(h), t)(_ :: _)
    }


}

object Variance {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] =
    for {
      m: Double <- mean(xs)
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    } yield v
}
