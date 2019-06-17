package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  // Exercise 3.1: What will be the result of the following match expression?

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  // Exercise 3.2: Implement the function tail for removing the first element of a List .
  // Note that the function takes constant time. What are different choices you could make
  // in your implementation if the List is Nil ?
  def tail[A](l: List[A]): List[A] = ???

  // Exercise 3.3: Implement the function setHead for replacing the first element
  //of a List with a different value
  def setHead[A](l: List[A], h: A): List[A] = ???

  // Exercise 3.4: Generalize tail to the function drop, which removes the first n elements from a list.
  def drop[A](l: List[A], n: Int): List[A] = ???

  // Exercise 3.5: Implement dropWhile , which removes elements from the List prefix as long as they
  //match a predicate.
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  // Exercise 3.6: Implement a function, init , that returns a List
  //consisting of all but the last element of a List .
  def init[A](l: List[A]): List[A] = ???

  // Exercise 3.7: Can product , implemented using foldRight , immediately halt the recursion and
  //return 0.0 if it encounters a 0.0 ?
  def productViaFoldRight(ns: List[Double]) = ???

  // Exercise 3.9: Compute the length of a list using foldRight
  def length[A](l: List[A]): Int = ???

  // Exercise 3.10: Implement the tail-recursive foldLeft
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  // Exercise 3.11: Write sum , product , and a function to compute the length of a list using foldLeft
  def sumViaFoldLeft(ns: List[Int]) = ???
  def productViaFoldLeft(ns: List[Double]) = ???
  def lengthViaFoldLeft[A](l: List[A]): Int = ???

  // Exercise 3.12: Write a function that returns the reverse of a list
  def reverse[A](l: List[A]) = ???

  // Exercise 3.13: Can you write foldLeft in terms of foldRight?
  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = ???

  // Exercise 3.14: Implement append in terms of either foldLeft or foldRight .
  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = ???

  // Exercise 3.15: Write a function that concatenates
  // a list of lists into a single list. Its runtime
  //should be linear in the total length of all lists.
  def concatenate[A](l: List[List[A]]): List[A] = ???

  // Exercise 3.18: Write a function map that generalizes
  // modifying each element in a list while maintaining the structure of the list.
  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  // Exercise 3.19: Write a function filter that removes elements
  // from a list unless they satisfy a given predicate.
  def filter[A](as: List[A])(f: A => Boolean): List[A] = ???

  // Exercise 3.20: Write a function flatMap that works
  // like map except that the function given will return
  //a list instead of a single result.
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = ???

  // Exercise 3.21: Use flatMap to implement filter
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = ???

  // Exercise 3.22: Write a function that accepts two lists
  // and constructs a new list by adding corresponding elements.
  def zipInts(as: List[Int], bs: List[Int]): List[Int] = ???

  // Exercise 3.23: Write a function that accepts two lists
  // and constructs a new list by adding corresponding elements.
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = ???

  // Exercise 3.24: Implement hasSubsequence for checking
  // whether a List contains another List as a subsequence
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

}