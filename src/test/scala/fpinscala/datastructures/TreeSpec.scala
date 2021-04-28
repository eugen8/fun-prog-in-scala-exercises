package fpinscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {

  behavior of "a Tree"

  it should "use Tree.size to calculate the size of the tree" in {
    Tree.size(Leaf(42)) should be(1)
    Tree.size(Branch(Leaf(1), Leaf(2))) should be(3)
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(5)
  }

  it should "use Tree.maximum to find the maximum value of the leaves" in {
    Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(3)
  }

  it should "use Tree.depth to calculate the maximum depth of a flat tree" in {
    Tree.depth(Branch(Leaf(1), Leaf(2))) should be(2)
  }

  it should "use Tree.depth to calculate the depth of an unbalanced tree" in {
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(3)
  }

  it should "use Tree.map to apply the function to all the elements" in {
    Tree.map(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))))(_ * 10) should be(Branch(Leaf(10), Branch(Branch(Leaf(20), Leaf(30)), Leaf(40))))
  }

}
