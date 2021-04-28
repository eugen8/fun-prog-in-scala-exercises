package fpinscala.testing

import fpinscala.state._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PropSpec extends AnyFlatSpec with Matchers {

  private val initialState = SimpleRNG(5L)

  behavior of "listOfN"

  it should "work in the static or dynamic case" in {
    val static = Gen.listOfN(10, Gen.boolean)
    val dynamic = Gen.boolean.listOfN(Gen.unit(10))

    static.sample.run(initialState) should be(dynamic.sample.run(initialState))
  }

  it should "work in the static or dynamic case with ints" in {
    val genInt = Gen.choose(Int.MinValue, Int.MaxValue)
    val static = Gen.listOfN(10, genInt)
    val dynamic = genInt.listOfN(Gen.unit(10))

    static.sample.run(initialState) should be(dynamic.sample.run(initialState))
  }

  behavior of "Prop &&"

  it should "combine two passing values into a single Passed" in {
    val expected: Prop = Prop.forAll(Gen.unit(true))(identity) && Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, 1, initialState) should be(Passed)
  }

  it should "combine one passing value and one failed value into a Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(true))(identity) && Prop.forAll(Gen.unit(false))(identity)

    expected.run(1, 1, initialState) should be(Falsified("false", 0))
  }

  it should "combine one failed value and one passing value into a Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) && Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, 1, initialState) should be(Falsified("false", 0))
  }

  it should "combine two failed values into a single Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) && Prop.forAll(Gen.unit(false))(identity)
    expected.run(1, 1, initialState) should be(Falsified("false", 0))
  }

  behavior of "Prop ||"

  it should "combine two passing values into a single Passed" in {
    val expected: Prop = Prop.forAll(Gen.unit(true))(identity) || Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, 1, initialState) should be(Passed)
  }

  it should "combine one passing value and one failed value into a Passed" in {
    val expected: Prop = Prop.forAll(Gen.unit(true))(identity) || Prop.forAll(Gen.unit(false))(identity)

    expected.run(1, 1, initialState) should be(Passed)
  }

  it should "combine one failed value and one passing value into a Passed" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) || Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, 1, initialState) should be(Passed)
  }

  it should "combine two failed values into a single Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) || Prop.forAll(Gen.unit(false))(identity)
    expected.run(1, 1, initialState) should be(Falsified("false", 0))
  }

  behavior of "SGen.listOf"

  it should "be the equivalent of Gen.listOfN, given the same list sizes" in {
    val static: Gen[List[Boolean]] = Gen.listOfN(10, Gen.boolean)
    val sGen: SGen[List[Boolean]] = SGen.listOf(Gen.boolean)

    static.sample.run(initialState) should be(sGen.forSize(10).sample.run(initialState))
  }

  behavior of "List.max"

  it should "find the maximum value" in {
    val smallInt = Gen.choose(-10,10)
    val maxProp: Prop = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.runForScalaTest(maxProp) should be(Passed)
  }

  behavior of "List.sorted"

  it should "sort the list" in {
    val sortedProp: Prop = Prop.forAll(SGen.listOf(Gen.choose(Int.MinValue, Int.MaxValue))) { as =>
      val sorted = as.sorted

/*
      sorted.foldLeft((true, sorted)) { case ((acc, list), i) =>
        (acc && !list.tail.exists(_ < i), list.tail)
      }._1
*/

      sorted.isEmpty || sorted.zip(sorted.tail).forall { case (i1, i2) => i1 <= i2 }
/*
      sorted.isEmpty || sorted.foldLeft((true, as.min)) { case ((acc, prev), i) =>
          (acc && prev <= i, i)
        }._1
*/
    }

    Prop.runForScalaTest(sortedProp) should be(Passed)
  }

  it should "correctly sort a list of doubles" in {
    val p = Prop.forAll(SGen.listOf1(Gen.double)) { ds =>
      ds.sorted.foldLeft((ds.min, true)) { case ((min, bool), d) =>
        (d, bool && d >= min)
      }._2
    }

    Prop.runForScalaTest(p) should be(Passed)
  }
}
