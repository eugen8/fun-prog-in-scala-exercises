package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {

  behavior of "scala.collection.immutable.List foldLeft"

  it should "build the list in reverse order" in {
    (0 to 10).foldLeft(scala.collection.immutable.List.empty[Int])((b, a) => a :: b) should be((0 to 10).toList.reverse)
  }


  behavior of "scala.collection.immutable.List foldRight"

  it should "build the list in order" in {
    (0 to 10).foldRight(scala.collection.immutable.List.empty[Int])(_ :: _) should be((0 to 10).toList)
  }


  behavior of "pattern matching"

  it should "equal 3" in {
    val exercise31 = fpinscala.datastructures.List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    exercise31 should be(3)
  }

  behavior of "tail"

  it should "return the tail of a non-empty list" in {
    val t = tail(List(1, 2, 3))

    t should be(List(2, 3))
  }

  it should "return Nil if the list is empty" in {
    val t = tail(Nil)

    t should be(Nil)
  }


  behavior of "setHead"

  it should "set the head of a non-empty list" in {
    val l = setHead(List(1, 2, 3), 42)

    l should be (List(42, 2, 3))
  }

  it should "set the head of an empty list" in {
    val l = setHead(Nil, 42)

    l should be (List(42))
  }

  behavior of "drop"

  it should "drop the leading elements of the list" in {
    val l = drop(List(1, 2, 3), 2)

    l should be(List(3))
  }

  it should "return Nil if it runs out of elements" in {
    val l = drop(List(1, 2, 3), 3)

    l should be(Nil)
    drop(l, 1) should be(Nil)
  }

  it should "return list if n is negative or zero" in {
    val list = List(1,2,3,4)
    val l = drop(list, -2)
    val l0 = drop(list, 0)

    l shouldBe list
    l0 shouldBe list
  }

  behavior of "dropWhile"

  it should "drop the elements of the list as long as the predicate is true" in {
    val l = dropWhile(List(1, 2, 3))(_ < 3)

    l should be(List(3))
  }

  it should "return Nil if it runs out of elements" in {
    val l = dropWhile(List(1, 2, 3))(_ => true)

    l should be(Nil)
  }

  behavior of "init"

  it should "drop the last element in the list" in {
    val l = init(List(1, 2, 3))

    l should be(List(1, 2))
  }

  behavior of "shortcircuitingFoldRight"

  it should "fold to 0" in {
    shortcircuitingFoldRight(List(0, 1), 1.0)(_ == 0)(_ * _) should be(0.0)
    shortcircuitingFoldRight(List(1, 0), 1.0)(_ == 0)(_ * _) should be(0.0)
  }

  behavior of "productShortCircuiting"
  it should "calculate product" in {
    productShortCircuiting(List(1,3,5,2,0,3,2)) should be(0.0)
    productShortCircuiting(List(1, 0)) should be(0.0)
    productShortCircuiting(List(0, 1)) should be(0.0)
    productShortCircuiting(List(1,3,5,2,1)) should be(30.0)
  }

  private def listOfLength(i: Int): List[Int] = (0 until i).foldRight(nil[Int])(Cons(_, _))

  behavior of "foldRight"

  it should "fold a list using +" in {
    foldRight(listOfLength(10), 0)(_ + _) should be(45)
  }

  it should "build a list when given Nil and Cons" in {
    foldRight(List(1, 2, 3), nil[Int])(Cons(_, _)) should be(List(1, 2, 3))
  }

  //  it should "blow up on large lists" in {
  //    a[StackOverflowError] should be thrownBy foldRight(listOfLength(10000), 0)(_ + _)
  //  }

  behavior of "foldLeft"

  it should "fold a list using +" in {
    foldLeft(listOfLength(10), 0)(_ + _) should be(45)
  }

  it should "build a list when given Nil and Cons" in {
    foldLeft(List(1, 2, 3), nil[Int])((b, a) => Cons(a, b)) should be(List(3, 2, 1))
  }

  it should "not blow up on large lists" in {
    foldLeft(listOfLength(10000), 0)(_ + _) should be(49995000)
  }

  behavior of "length"

  it should "return the length of the list" in {
    len(List(1, 2, 3)) should be(3)
    len(nil[String]) should be (0)
  }

  behavior of "sumLeft"

  it should "add the numbers" in {
    sumLeft(List(1, 2, 3)) should be(6)
  }

  behavior of "productLeft"

  it should "multiply the numbers" in {
    productLeft(List(1, 2, 3)) should be(6)
  }

  "lenLeft" should "compute the length of the list" in {
    lenLeft(Nil) should be(0)
    lenLeft(listOfLength(0)) should be(0)
    lenLeft(listOfLength(42042)) should be(42042)
  }

  behavior of "reverse"

  it should "reverse the list" in {
    reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  behavior of "foldRightViaFoldLeft"

  it should "fold a list using +" in {
    foldRightViaFoldLeft(listOfLength(10), 0)(_ + _) should be(45)
  }

  it should "build a list when given Nil and Cons" in {
    foldRightViaFoldLeft(List(1, 2, 3), nil[Int])(Cons(_, _)) should be(List(1, 2, 3))
  }

  it should "not blow up on large lists" in {
    foldRightViaFoldLeft(listOfLength(10000), 0)(_ + _) should be(49995000)
  }

  behavior of "foldLeftViaFoldRight"

  it should "fold a list using +" in {
    foldLeftViaFoldRight(listOfLength(10), 0)(_ + _) should be(45)
  }

  it should "build a list when given Nil and Cons" in {
    foldLeftViaFoldRight(List(1, 2, 3), nil[Int])((b, a) => Cons(a, b)) should be(List(3, 2, 1))
  }

  //  it should "not blow up on large lists" in {
  //    a[StackOverflowError] should be thrownBy foldLeftViaFoldRight(listOfLength(10000), 0)(_ + _)
  //  }

  behavior of "append"

  it should "append two lists in order" in {
    append(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
  }

  it should "handle Nil on the left" in {
    append(nil[Int], List(1, 2, 3)) should be(List(1, 2, 3))
  }

  it should "handle Nil on the right" in {
    append(List(1, 2, 3), nil[Int]) should be(List(1, 2, 3))
  }

  behavior of "flatten"

  it should "flatten the lists" in {
    flatten(List(List(1, 2, 3), List(4, 5, 6))) should be(List(1, 2, 3, 4, 5, 6))
  }

  behavior of "mapPlus1"

  it should "increment the numbers" in {
    mapPlus1(List(1, 2, 3)) should be (List(2, 3, 4))
  }

  behavior of "doublesToStrings"

  it should "convert the doubles to strings" in {
    doublesToStrings(List(1.0, 2.0, 3.14)) should be(List("1.0", "2.0", "3.14"))
  }

  behavior of "map"

  it should "increment numbers" in {
    map(List(1, 2, 3))(_ + 1) should be(List(2, 3, 4))
  }

  it should "convert doubles to strings" in {
    map(List(1.0, 2.0, 3.14))(_.toString) should be(List("1.0", "2.0", "3.14"))
  }

  behavior of "filter"

  it should "remove odd numbers from the list" in {
    filter(List(1, 2, 3, 4))(_ % 2 == 0) should be(List(2, 4))
  }

  it should "remove even numbers from the list" in {
    filter(List(1, 2, 3, 4))(_ % 2 == 1) should be(List(1, 3))
  }

  it should "handle empty lists" in {
    filter(nil[Int])(_ % 2 == 0) should be(nil[Int])
  }

  behavior of "flatMap"

  it should "work like map.flatten" in {
    val input = List(1, 2, 3)

    val mapped = map(input)(List(_))
    mapped should be(List(List(1), List(2), List(3)))

    flatten(mapped) should be(flatMap(input)(List(_)))
  }

  behavior of "filterWithFlatMap"

  it should "remove odd numbers from the list" in {
    filterWithFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) should be(List(2, 4))
  }

  it should "remove even numbers from the list" in {
    filterWithFlatMap(List(1, 2, 3, 4))(_ % 2 == 1) should be(List(1, 3))
  }

  it should "handle empty lists" in {
    filterWithFlatMap(nil[Int])(_ % 2 == 0) should be(nil[Int])
  }

  behavior of "zipAdd"

  it should "add the corresponding elements of two lists of integers" in {
    zipAdd(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
  }

  it should "drop treat missing elements" in {
    zipAdd(List(1, 2, 3), List(4, 5)) should be (List(5, 7))
    zipAdd(List(1, 2), List(4, 5, 6)) should be (List(5, 7))
  }

  behavior of "zipWith"

  it should "add the corresponding elements of two lists of integers" in {
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be(List(5, 7, 9))
  }

  it should "drop missing elements" in {
    zipWith(List(1, 2, 3), List(4, 5))(_ + _) should be (List(5, 7))
    zipWith(List(1, 2), List(4, 5, 6))(_ + _) should be (List(5, 7))
  }

  behavior of "hasSubsequence"

  it should "work using the book's examples" in {
    val input = List(1,2,3,4)
    hasSubsequence(input, List(1,2)) should be(true)
    hasSubsequence(input, List(2,3)) should be(true)
    hasSubsequence(input, List(4)) should be(true)
  }

  it should "work using some other examples" in {
    val input = List(1,2,3,4)
    hasSubsequence(input, List(42)) should be(false)
    hasSubsequence(input, List(4, 3)) should be(false)
    hasSubsequence(input, nil) should be(true)
  }

  it should "not include gaps" in {
    hasSubsequence(List(1, 2, 3, 4), List(1, 3)) should be(false)
    hasSubsequence(List(1, 2, 3, 4), List(2, 4)) should be(false)
  }

}
