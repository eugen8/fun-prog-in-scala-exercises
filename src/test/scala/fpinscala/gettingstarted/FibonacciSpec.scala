package fpinscala.gettingstarted

import fpinscala.gettingstarted.Fibonacci._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FibonacciSpec extends AnyFlatSpec with Matchers {

  behavior of "fib function"

  it should "return 0 for fib(0)" in {
    fib(0) should be(0)
  }

  it should "return 1 for fib(1)" in {
    fib(1) should be(1)
  }

  it should "return 1 for fib(2)" in {
    fib(2) should be(1)
  }

  it should "return 2 for fib(3)" in {
    fib(3) should be(2)
  }

  it should "return 5 for fib(5)" in {
    fib(5) should be(5)
  }

  it should "return 102334155 for fib(40)" in {
    fib(40) should be(102334155)
  }

  it should "return 1100087778366101931 for fib(88)" in {
    fib(88) should be(1100087778366101931L)
  }

  it should "return 4660046610375530309 for fib(91)" in {
    fib(91) should be(4660046610375530309L)
  }

  it should "return 7540113804746346429 for fib(92)" in {
    fib(92) should be(7540113804746346429L)
  }

}
