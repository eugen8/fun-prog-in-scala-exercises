package fpinscala.laziness

import fpinscala.laziness.Stream.cons
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StreamSpec extends AnyFlatSpec with Matchers {

  behavior of "takeWhile"

  def failInt: Int =
    throw new RuntimeException("This should not have been evaluated")

  it should "take things until the predicate fails" in {
    //    Stream(1, 2, 1).takeWhile(i => i % 2 == 1).toList should be(List(1))
    cons(1, cons(2, cons(failInt, Stream.empty[Int]))).takeWhile(i => i % 2 == 1).toList should be(List(1))
  }

  behavior of "forAll"

  it should "return true if the predicate is true for all elements" in {
    Stream(1, 2).forAll(_ < 3) should be(true)
  }

  it should "return false if the predicate is false for any elements" in {
    Stream(1, 2).forAll(_ > 3) should be(false)
  }

  it should "return false for Ryan's test" in {
    Stream(1, 2).forAll(_ < 2) should be(false)
  }

  behavior of "fibs"

  it should "return the first 7 elements" in {
    Stream.fibs.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  it should "return the first 7 elements implemented with unfold" in {
    Stream.fibUnfold.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  behavior of "mapUnfold"

  it should "apply the function" in {
    Stream(1, 2).mapUnfold(_ * 2).toList should be(List(2, 4))
  }

  behavior of "startsWith"

  it should "implement the example" in {
    val (a, b) = Stream.unzip(Stream(1, 2, 3).zipAll(Stream(1, 2)))

    println((a.toList, b.toList))
    Stream(1, 2, 3).startsWith(Stream(1, 2)) should be(true)
  }

  it should "return false if it doesn't start with the substream" in {
    Stream(2, 3).startsWith(Stream(1, 2)) should be(false)
  }

  it should "only return true if the original stream is longer than the substream" in {
    Stream(1, 2).startsWith(Stream(1, 2, 3)) should be(false)
  }

  it should "work for infinite base streams" in {
    Stream.ones.startsWith(Stream(1)) should be(true)
  }

  it should "return false if it detects a mismatch before infinity" in {
    Stream.ones.startsWith(Stream.constant(2)) should be(false)
  }

  // can't really do this:
  //  it should "work for infinite streams" in {
  //    Stream.ones.startsWith(Stream.ones) should be(true)
  //  }

  behavior of "tails"

  it should "implement the example" in {
    Stream(1, 2, 3).tails.map(_.toList).toList
      .should(be(Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream()).map(_.toList).toList))
  }

  it should "work with infinite streams" in {
    val value1: Stream[Stream[Int]] = Stream.from(1).tails.map(_.take(2)).take(3)
    value1.map(_.toList).toList should be(List(List(1, 2), List(2, 3), List(3, 4)))
  }

  behavior of "scanRight"

  it should "implement the example" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList should be(List(6, 5, 3, 0))
  }

//  it should "work on infinite streams" in {
//    Stream.from(1).scanRight(0)(_ + _).take(5).toList should be(List(15, 10, 6, 3, 1, 0))
//  }
}
