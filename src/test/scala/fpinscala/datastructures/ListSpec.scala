package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class ListSpec extends FlatSpec with Matchers {

  "tail" should "behave like tail" in {
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)
    List.tail(List()) shouldBe List()
  }

  "setHead" should "change the head element if exists" in {
    List.setHead(List(1, 2, 3), 5) shouldBe List(5, 2, 3)
    List.setHead(List(), 1) shouldBe List()
  }

//  "takeWhile" should "behave like takeWhile" in {
//
//    val f: Int => Boolean = _ < 3
//
//    List.takeWhile(List(1, 2, 3), f) shouldBe List(1, 2)
//    List.takeWhile(List(1), f) shouldBe List(1)
//    List.takeWhile(List(), f) shouldBe List()
//  }

  "dropWhile" should "behave like dropWhile" in {

    val f: Int => Boolean = _ < 3

    dropWhile(List(1, 2, 3), f) shouldBe List(3)
    dropWhile(List(3, 2, 1), f) shouldBe List(3, 2, 1)
    dropWhile(List(), f) shouldBe List()
  }

  "init" should "remove last element of the list" in {
    init(List(1, 2, 3)) shouldBe List(1, 2)
    init(List()) shouldBe List()
  }

  "length" should "count the elemts of the list" in {
    List.length(List(1, 2, 3)) shouldBe 3
    List.length(List()) shouldBe 0
  }

  behavior of "foldLeft"

  it should "sum" in {
    def add(x: Int, y: Int) = x + y

    foldLeft[Int, Int](List(1, 2, 3), 0)(add) shouldBe 6
    foldLeft[Int, Int](List(), 0)(add) shouldBe 0
  }

  it should "product" in {
    val X: (Int, Int) => Int = _ * _
    foldLeft[Int, Int](List(1, 2, 3), 0)(X) shouldBe 0
    foldLeft[Int, Int](List(1, 2, 3), 1)(X) shouldBe 6
    foldLeft[Int, Int](List(), 1)(X) shouldBe 1
  }

  "map" should "replace all elements in the list by the given function" in {
    val X: (Int, Int) => Int = _ * _
    val x3: Int => Int = X(3, _)

    map(List(1, 2, 3))(x3) shouldBe List(3, 6, 9)
    map(List())(x3) shouldBe List()
  }

//  "reduce" should "be similar to fold* but the in and out types are the same" in {
//    reduce(List(1, 2, 3), 0)((x, y) => x + y) shouldBe 6
//  }

  "filter" should "filter by a " in {
    val even: Int => Boolean = _ % 2 == 0

    filter(List(1, 2, 3))(even) shouldBe List(2)
    filter[Int](List(1, 2, 3))(_ > 5) shouldBe List()
    filter[Int](List(1, 2, 3))(_ < 5) shouldBe List(1, 2, 3)
    filter[Int](List())(_ < 5) shouldBe List()
  }

  "append" should "concatenate" in {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    append(l1, l2) shouldBe List(1, 2, 3, 4, 5, 6)
  }

//  "flatten"  should "flatten" in {
//    flatten(List(List(1, 2), List(3))) shouldBe List(1, 2, 3)
//    flatten(List(List(), List(3))) shouldBe List(3)
//    flatten(List(List(), List(2, 3))) shouldBe List(2, 3)
//    flatten(List(List(1, 2), List())) shouldBe List(1, 2)
//    flatten(List(List(1), List())) shouldBe List(1)
//    flatten(List(List(), List())) shouldBe List()
//  }


  behavior of "flatMap"

  it should "boxing" in {
    flatMap[String, Int](List("1", "22"))(s => List(s.length)) shouldBe List(1, 2)
    flatMap[List[Int], Int](List(List(1, 2), List(3)))(identity) shouldBe List(1, 2, 3)
    flatMap[Int, Int](List(1, 2))(a => List(a, a * a)) shouldBe List(1, 1, 2, 4)
  }

  it should "do magic" in {
    val numbers = List(1, 2, 3)
    val chars = List('a', 'b')

    val fm = flatMap(numbers)(num =>
      map(chars)(ch =>
        (ch, num)
      )
    )

    fm shouldBe List(
      ('a', 1), ('b', 1),
      ('a', 2), ('b', 2),
      ('a', 3), ('b', 3)
    )
  }

//  "partition" should "partition elements to a Pair of Lists using a predicate" in {
//    val list = List(1, 2, 3, 4)
//    val predicate: Int => Boolean = _ % 2 == 0
//
//    partition(list, predicate) shouldBe(List(2, 4), List(1, 3))
//    partition(List(), predicate) shouldBe(List(), List())
//    partition(List(1, 3), predicate) shouldBe(List(), List(1, 3))
//  }

  behavior of "List"

  it should "avg with foldLeft" in {

    def avg(nums: List[Double], default: Double = 0.0): Double = nums match {
      case Nil => default
      case _ =>
        val (sum, count) = foldLeft(map(nums)((_, 1)), (0.0, 0))((aggr, actual) => (aggr._1 + actual._1, aggr._2 + 1))
        sum / count
    }

    avg(List(1, 2, 3)) shouldBe 2
    avg(List(1, 2, 3, 4)) shouldBe 2.5
    avg(List()) shouldBe 0
  }

  "examples" should "avg with foldLeft" in {

    def fact_not_tailrec(i: Int): Int =
      if (i < 2) 1
      else i * fact(i - 1)

    def fact(i: Int): Int = {

      @tailrec
      def go(x: Int, aggr: Int): Int =
        if (x < 2) aggr
        else go(x - 1, x * aggr)

      go(i, 1)
    }

    fact(1) shouldBe 1
    fact(2) shouldBe 2
    fact(3) shouldBe 6
    fact(4) shouldBe 24
    fact(5) shouldBe 120

  }

  "zipWith" should "zip 2 lists" in {
    zipWith(List(1, 2, 3), List("a", "b", "c"))((_, _)) shouldBe List((1, "a"), (2, "b"), (3, "c"))
    zipWith(List(1, 2), List("a", "b", "c"))((_, _)) shouldBe List((1, "a"), (2, "b"))
    zipWith(List(1, 2, 3), List("a", "b"))((_, _)) shouldBe List((1, "a"), (2, "b"))
    zipWith(List[Int](), List("a", "b", "c"))((_, _)) shouldBe List()
    zipWith(List(1, 2, 3), List())((_, _)) shouldBe List()

    map[(Int, Int), Int](
      zipWith(List(1, 2, 3), List(4, 5, 6))((_, _))
    )(e => e._1 * e._2) shouldBe List(4, 10, 18)
  }

  "hasSubsequence" should "..." in {
    hasSubsequence(List(1, 2, 3), List()) shouldBe true
    hasSubsequence(List(1, 2, 3), List(1, 2)) shouldBe true
    hasSubsequence(List(1, 2, 3), List(2, 3)) shouldBe true
    hasSubsequence(List(1, 2, 3), List(2, 1)) shouldBe false
    hasSubsequence(List(1, 2, 3), List(1, 3)) shouldBe false
    hasSubsequence(List(), List()) shouldBe true
    hasSubsequence(List(), List(1)) shouldBe false
  }
}
