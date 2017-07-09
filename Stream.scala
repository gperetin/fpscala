sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): List[A] = this match {
    case Cons(h, t) if n > 0 => h() :: t().take(n-1)
    case _ => List.empty
  }

  def drop(n: Int): List[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case Cons(h, t) => h() :: t().toList
    case Empty => List.empty
  }

  // Return all starting elements that match the given predicate
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  // takeWhile using foldRight
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
        if (p(h)) Stream.cons(h, t)
        else Stream.empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // Check that all elements in the stream match a given predicate
  // Terminate early for first false element
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
        if (f(h)) Stream.cons(h, t)
        else t)

  def append[B>:A](b: => Stream[B]): Stream[B] =
    foldRight(b)((h, t) => Stream.cons(h, t))


  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h) append t)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
