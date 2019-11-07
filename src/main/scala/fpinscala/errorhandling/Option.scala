package fpinscala.errorhandling

import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

//you should be able to implement all the
//functions besides and without resorting to pattern matching

sealed trait Option[+A] {

  //  EXERCISE 1
  def map[B](f: A => B): Option[B] = ???
  //  returns the result inside the Some case of the Option,
  //  or if the Option is None returns the given default value
  def getOrElse[B>:A](default: => B): B = ???
  def flatMap[B](f: A => Option[B]): Option[B] = ???
  //   returns the first Option if it is defined, otherwise, returns the second Option.
  def orElse[B>:A](ob: => Option[B]): Option[B] = ???
  def filter(f: A => Boolean): Option[A] = ???
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //  EXERCISE 2
  def variance(xs: Seq[Double]): Option[Double] = ???

  //  EXERCISE 3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  //  EXERCISE 5
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  //  EXERCISE 6
  //  It is straightforward to do using map and sequence,
  //  but try for a more efficient implementation that only looks at the sequence
  //    list once.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

  //  EXERCISE 6b
  //  implement sequence in terms of traverse.
}
