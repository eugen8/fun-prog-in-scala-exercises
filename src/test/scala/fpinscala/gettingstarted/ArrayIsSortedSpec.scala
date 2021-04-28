package fpinscala.gettingstarted

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrayIsSortedSpec extends AnyFlatSpec with Matchers {

  behavior of "isSorted"

  it should "return true when, for each pair of consecutive ints elements, the ordered function returns true" in {
    val input = Array(0, 1, 2, 3)

    ArrayIsSorted.isSorted(input)(_ < _) should be(true)
  }

  it should "return false when, for any pair of consecutive ints elements, the ordered function returns false" in {
    val input = Array(0, 2, 1, 3)

    ArrayIsSorted.isSorted(input)(_ < _) should be(false)
  }

  it should "return true when, for each pair of consecutive string elements, the ordered function returns true" in {
    val input = Array("A", "B", "C")

    ArrayIsSorted.isSorted(input)(_ < _) should be(true)
  }

  it should "return false when, for any pair of consecutive string elements, the ordered function returns false" in {
    val input = Array("B", "A", "C")

    ArrayIsSorted.isSorted(input)(_ < _) should be(false)
  }



}
