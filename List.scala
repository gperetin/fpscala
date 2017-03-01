
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](elems: List[A]): List[A] = elems match {
    case Nil => Nil
    case Cons(x, tail) => tail
  }

  def drop[A](l: List[A], n:Int): List[A] = {
    def go(l: List[A], n:Int): List[A] = {
      if (n == 0) l
      else go(tail(l), n - 1)
    }

    go(l, n)
  }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (p(x)) dropWhile(xs, p)
      else l
  }

  def setHead[A](elems:List[A], newElem:A): List[A] = elems match {
    case Nil => List(newElem)
    case Cons(x, xs) => Cons(newElem, xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}