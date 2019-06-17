package fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}

class GettingStartedSpec extends FlatSpec with Matchers {

  behavior of "MyModule.fib"

  it should "calculate the Fibonacci numbers" in {
    MyModule.fib(0) shouldBe 0
    MyModule.fib(1) shouldBe 1
    MyModule.fib(2) shouldBe 1
    MyModule.fib(3) shouldBe 2
    MyModule.fib(4) shouldBe 3
    MyModule.fib(5) shouldBe 5
    MyModule.fib(6) shouldBe 8
    MyModule.fib(7) shouldBe 13
    MyModule.fib(8) shouldBe 21
    MyModule.fib(9) shouldBe 34
  }

  behavior of "PolymorphicFunctions.isSorted"

  it should "sort Ints" in {
    PolymorphicFunctions.isSorted[Int](Array(1, 2, 3), _ < _) shouldBe true
    PolymorphicFunctions.isSorted[Int](Array(1, 2, 3), _ <= _) shouldBe true
    PolymorphicFunctions.isSorted[Int](Array(2, 2, 3), _ <= _) shouldBe true
    PolymorphicFunctions.isSorted[Int](Array(), _ < _) shouldBe true
    PolymorphicFunctions.isSorted[Int](Array(1, 2, 3), _ > _) shouldBe false
    PolymorphicFunctions.isSorted[Int](Array(1, 2, 1), _ > _) shouldBe false
  }

  it should "sort Chars" in {
    PolymorphicFunctions.isSorted[Char](Array('a', 'b', 'c'), _ < _) shouldBe true
    PolymorphicFunctions.isSorted[Char](Array('a', 'b', 'c'), _ <= _) shouldBe true
    PolymorphicFunctions.isSorted[Char](Array('b', 'b', 'c'), _ <= _) shouldBe true
    PolymorphicFunctions.isSorted[Char](Array(), _ < _) shouldBe true
    PolymorphicFunctions.isSorted[Char](Array('a', 'b', 'c'), _ > _) shouldBe false
    PolymorphicFunctions.isSorted[Char](Array('a', 'b', 'a'), _ > _) shouldBe false
  }

  behavior of "PolymorphicFunctions.curry"

  it should "curry" in {
    val f: (Int, String) => Double = _ / _.toDouble

    val ff = PolymorphicFunctions.curry[Int, String, Double](f)

    val str2Double= ff(8)
    str2Double("3") shouldBe 8.0 / 3
  }

  behavior of "PolymorphicFunctions.uncurry"

  it should "uncurry" in {
    val str2Double: String => Double = _.toDouble
    val f: Int => String => Double = a => b => a * b.toDouble

    val ff = PolymorphicFunctions.uncurry[Int, String, Double](f)

    ff(2, "3") shouldBe 6.0
  }

  behavior of "PolymorphicFunctions.compose"

  it should "compose" in {
    val multi3: String => Double = _.toDouble * 3
    val plus2: Int => String = a => (a + 2).toString

    val ff = PolymorphicFunctions.compose[Int, String, Double](multi3, plus2)

    ff(8) shouldBe 30.0
  }

}
