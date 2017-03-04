sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    def go[A](t: Tree[A]): Int = t match {
        case Branch(left, right) => go(left) + 1 + go(right)
        case Leaf(_) => 1
      }
    go(tree)
  }

  def maximum(tree: Tree[Int]): Int = {
    def go(t: Tree[Int]): Int = t match {
      case Branch(left, right) => go(left) max go(right)
      case Leaf(value) => value
    }
    go(tree)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(t: Tree[A]): Int = t match {
      case Branch(left, right) => go(left) max go(right) + 1
      case Leaf(_) => 1
    }
    go(tree)
  }

  def map(tree: Tree[Int], f: Int => Int): Tree[Int] = {
    def go(t: Tree[Int]): Tree[Int] = t match {
      case Branch(left, right) => Branch(go(left), go(right))
      case Leaf(value) => Leaf(f(value))
    }
    go(tree)
  }
}
