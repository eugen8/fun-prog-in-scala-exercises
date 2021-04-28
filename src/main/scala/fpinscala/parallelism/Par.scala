package fpinscala.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

/* Definitions from java.util.concurrent._:

      class ExecutorService {
        def submit[A](a: Callable[A]): Future[A] = ???
      }
      trait Callable[A] { def call: A }
      trait Future[A] {
        def get: A
        def get(timeout: Long, unit: TimeUnit): A
        def cancel(evenIfRunning: Boolean): Boolean
        def isDone: Boolean
        def isCancelled: Boolean
      }
 */

sealed trait Future[+A] {
  private[parallelism] def apply(cb: A => Unit): Unit //cb is callback
}

object Par {
  type Par[+A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => new Future[A] {
    override private[parallelism] def apply(cb: A => Unit): Unit =
      cb(a)
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])
                   (f: (A, B) => C): Par[C] = es =>
    new Future[C] {
      override private[parallelism] def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
          case Right(b) =>
            ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }
    }

  def fork[A](a: => Par[A]): Par[A] = es =>
    new Future[A] {
      override private[parallelism] def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit = {
    es.submit(new Callable[Unit] { def call: Unit = r })

    ()
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)
            (p: Par[A]): A = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)

    p(es) { a =>
      ref.set(a)
      latch.countDown()
    }

    latch.await()
    ref.get
  }

  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A,B](pa: Par[A])
              (f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))
//    ps.foldRight(lazyUnit(List.empty[A])) { (parA, pas) =>
//      map2(parA, pas)( { (a, l) =>
//        a :: l
//      })
//    }

  def parMap[A, B](ps: List[A])
                  (f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])
                  (f: A => Boolean): Par[List[A]] = {
    val parF: A => Par[Boolean] = asyncF(f)

    // sequence version
//    val step1: List[Par[(A, Boolean)]] = as.map { a =>
//      val parBoolean = parF(a)
//      map2(unit(a), parBoolean)(_ -> _)
//    }
//
//    val step2: Par[List[(A, Boolean)]] = sequence(step1)
//
//    val output: Par[List[A]] = map(step2)(_.filter(_._2).map(_._1))
//    output

    // raw foldRight version
    as.foldRight(lazyUnit(List.empty[A])) { (a: A, parAs: Par[List[A]]) =>
      map2(parF(a), parAs) { (b, as) =>
        if (b) a :: as else as
      }
    }
  }

  def choice[A](cond: Par[Boolean])
               (t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)
//    choiceN(map(cond) {
//      case true => 0
//      case false => 1
//    })(List(t, f))
//    es =>
//      if (run(es)(cond)) t(es)
//      else f(es)
/*
    choiceN(map(cond) {
      case true => 0
      case false => 1
    })(List(t, f))
*/

  def choiceN[A](n: Par[Int])
                (choices: List[Par[A]]): Par[A] =
    chooser(n)(choices.apply)
//  { es =>
//    val x: Int = run(es)(n)
//
//    val pa: Par[A] = choices(x)
//
//    pa(es)
//  }
/*
    es =>
      choices(run(es)(n))(es)
*/

  def choiceMap[K, V](key: Par[K])
                     (choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(choices.apply)

  //  { es =>
//    val x: K = run(es)(key)
//
//    val pv: Par[V] = choices(x)
//
//    pv(es)
//  }
/*
    es =>
      choices(run(es)(key))(es)
*/

  def chooser[A, B](pa: Par[A])
                   (choices: A => Par[B]): Par[B] =
//    join(map(pa)(choices))
    flatMap(pa)(choices)
/*
    es =>
      choices(run(es)(pa))(es)
*/

  def flatMap[A, B](a: Par[A])
                   (f: A => Par[B]): Par[B] =
    join(map(a)(f))
//  { es =>
//    f(run(es)(a))(es)
//  }

  /*
      es =>
        f(run(es)(a))(es)
  */
/*
    join(map(a)(f))
*/

// A = Par[Z]
// B = Z
  // f = Par[Z] => Par[Z]
  def join[Z](a: Par[Par[Z]]): Par[Z] = { es =>
//      identity(run(es)(a))(es)
      run(es)(a)(es)
  }

//    flatMap(a)(identity)
/*
    flatMap(a)(identity)
*/
/*
    es =>
      run(es)(a)(es)
*/
}

object Tester extends App {
  import Par._
  val s = Executors.newFixedThreadPool(20)

  private val slowIncrement: Int => Par[Int] = asyncF { i =>
    println(s"asyncF start $i on ${Thread.currentThread().getName}")
    Thread.sleep(1000L)
    println(s"asyncF end $i")
    i + 1
  }

  def doFirstExample(): Unit = {
    val value: Par[Int] = map2(slowIncrement(20), fork(unit {
      println(s"fast start on ${Thread.currentThread().getName}")
      21
    }))(_ + _)

    println(s"starting on ${Thread.currentThread().getName}")
    println(run(s)(value))
    println("finishing")
  }

  def doSequence(): Unit = {
    println("starting sequence")
    val parInts: List[Par[Int]] = (0 until 10).map(slowIncrement).toList
    println("we have a list of Par[Int]")
    val parListInt: Par[List[Int]] = sequence(parInts)
    println("we have a Par[List[Int]]")
    val ints: List[Int] = run(s)(parListInt)
    println(ints)
    println("finished sequence")
  }

  def doParFilter(): Unit = {
    println("starting parFilter")
    val ints: List[Int] = (0 until 10).toList

    val slowF: Int => Boolean = i => {
      println(s"starting filter $i on ${Thread.currentThread().getName}")
      Thread.sleep(1000L)
      println(s"finishing filter $i")
      i % 2 == 0
    }

    println(run(s)(parFilter(ints)(slowF)))
    println("finished parFilter")
  }


//  run(s)(map2(slowIncrement(1), slowIncrement(2))(_ + _))

//  doFirstExample()

//  doSequence()

  doParFilter()

  s.shutdown()
}
