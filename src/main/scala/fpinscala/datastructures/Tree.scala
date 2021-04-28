package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ + _ + 1)

  def maximum(tree: Tree[Int]): Int =
    fold(tree)(identity)(_ max _)

  def depth[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ max _ + 1)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}
