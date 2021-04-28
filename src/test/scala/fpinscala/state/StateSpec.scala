package fpinscala.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateSpec extends AnyFlatSpec with Matchers {
  val rng: RNG = SimpleRNG(42L)

  val firstInt: Int = 16159453
  val secondInt: Int = -1281479697
  val thirdInt: Int = -340305902
  val fourthInt: Int = -2015756020
  val fifthInt: Int = 1770001318
  val firstDouble: Double = 0.007524831686168909
  val secondDouble: Double = 0.8242210922762752
  val thirdDouble: Double = 0.4730720594525337

  behavior of "SimpleRNG"

  it should "return values based on the seed" in {
    val (n1, rng2) = rng.nextInt

    val (n2, _) = rng2.nextInt

    n1 should be (firstInt)
    n2 should be (secondInt)
  }

  behavior of "nonNegativeInt"

  it should "return a non-negative integer" in {
    val (_, rng) = SimpleRNG(42L).nextInt

    val (output, _) = RNG.nonNegativeInt.run(rng)

    output should be (fifthInt)
  }

  it should "handle the Int.MinValue corner case" in {
    val (output, _) = RNG.nonNegativeInt.run(new RNG {
      override def nextInt: (Int, RNG) = (Int.MinValue, SimpleRNG(42L))
    })

    output should be (firstInt)
  }

  behavior of "double"

  it should "return a double between 0 and 1" in{
    val (output, _) = RNG.double.run(rng)

    output should be(firstDouble)
  }

  it should "handle the Int.MaxValue corner case" in {
    val (output, _) = RNG.double.run(new RNG {
      override def nextInt: (Int, RNG) = (Int.MaxValue, SimpleRNG(42L))
    })

    output should be(0.9999999995343387)
  }

  behavior of "intDouble"

  it should "return an int and a double" in {
    val ((i, d), _) = RNG.intDouble.run(rng)

    i should be(firstInt)
    d should be(0.8242210922762752)
  }

  behavior of "doubleInt"

  it should "return an int and a double" in {
    val ((d, i), _) = RNG.doubleInt.run(rng)

    d should be(0.007524831686168909)
    i should be(secondInt)
  }

  behavior of "double3"

  it should "return 3 doubles" in {
    val ((d1, d2, d3), _) = RNG.double3.run(rng)

    d1 should be(firstDouble)
    d2 should be(secondDouble)
    d3 should be(thirdDouble)
  }

  behavior of "ints"

  it should "return the given number of ints" in {
    val (output, _) = RNG.ints(3).run(rng)

    output should be(List(firstInt, secondInt, thirdInt))
  }

  it should "return an RNG in the correct state" in {
    val (_, output) = RNG.ints(3).run(rng)
    val (nextInt, _) = output.nextInt

    nextInt should be(fourthInt)
  }

  behavior of "nonNegativeLessThan"

  it should "return a non-negative number less than the given argument" in {
    // this seed will result in a nextInt that should cause nonNegativeLessThan to recurse once when called with 17
    val seed = 140737488196451L
    val particularRNG = SimpleRNG(seed)

    val boundary = 17
    val (i, r) = RNG.nonNegativeLessThan(boundary).run(particularRNG)
    val (nextInt, _) = r.nextInt

    i should (be < boundary and be >= 0)
    nextInt shouldEqual particularRNG.nextInt._2.nextInt._1
  }

  behavior of "rollDie"

  it should "not return 0 when initialized with the known buggy seed" in {
    val (i, _) = RNG.rollDie.run(SimpleRNG(5))

    i should (be >= 1 and be <= 6)
  }
}
