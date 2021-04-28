package fpinscala.errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    flatMap(f.andThen(Right(_)))
//  def map[B](f: A => B): Either[E, B] = this match {
//    case Left(e) => Left(e)
//    case Right(a) => Right(f(a))
//  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(r) => f(r)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, Z](as: List[A])
                       (f: A => Either[E, Z]): Either[E, List[Z]] =
    as.foldRight(Right(List.empty[Z]): Either[E, List[Z]]) { (h, t) =>
      f(h).map2(t)(_ :: _)
    }

}
