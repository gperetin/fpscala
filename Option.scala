import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
object None extends Option[Nothing]

object Option {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
  }
}