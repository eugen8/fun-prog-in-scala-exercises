package fpinscala.state

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }

  def map2[B, C](bs: State[S, B])
                (f: (A, B) => C): State[S, C] =
    State.map2(this, bs)(f)
}

object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))
  def map2[S, A, B, C](as: State[S, A], bs: State[S, B])
                      (f: (A, B) => C): State[S, C] =
    for {
      a <- as
      b <- bs
    } yield f(a, b)

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight[State[S, List[A]]](unit(List.empty[A]))(map2(_, _)(_ :: _))

  def sequence[S, A](fs: LazyList[State[S, A]]): State[S, LazyList[A]] =
    fs.foldRight[State[S, LazyList[A]]](unit(LazyList.empty[A]))(map2(_, _)(_ #:: _))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  val int: Rand[Int] = State(_.nextInt)

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def nonNegativeInt: Rand[Int] = State { rng =>
    val (i, r) = rng.nextInt

    if (i < 0) nonNegativeInt.run(r) else (i, r)
  }

  def double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt: Rand[(Double, Int)] =
  both(double, int)

  def double3: Rand[(Double, Double, Double)] = State { rng =>
    val (d1, r1) = double.run(rng)
    val (d2, r2) = double.run(r1)
    val (d3, r3) = double.run(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(int))

  type Rand[+A] = State[RNG, A]

  def unit[A](a: A): Rand[A] =
/*
    State((a, _))
*/
    State.unit(a)

  def map[A, B](s: Rand[A])
               (f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
/*
    flatMap(s)(a => unit(f(a)))
*/
    s.map(f)

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])
                   (f: (A, B) => C): Rand[C] =
//    rng => {
//      val (a, r1) = ra(rng)
//      val (b, r2) = rb(r1)
//
//      (f(a, b), r2)
//    }
    /*flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))*/
    State.map2(ra, rb)(f)

  def sequence[A](fs: LazyList[Rand[A]]): Rand[LazyList[A]] =
//    fs.foldRight(unit(LazyList.empty[A]))(map2(_, _)(_ #:: _))
    State.sequence(fs)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
//    fs.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))
    State.sequence(fs)

  def nonNegativeLessThan(n: Int): Rand[Int] =
//    rng => {
//      val (i, rng2) = nonNegativeInt(rng)
//      val mod = i % n
//      if (i + (n - 1) - mod >= 0) (mod, rng2)
//      else nonNegativeLessThan(n)(rng2)
//    }
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
//    State { rng =>
//      val (a, r) = f.run(rng)
//      g(a).run(r)
//    }
    f flatMap g
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
