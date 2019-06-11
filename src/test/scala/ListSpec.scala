import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

import scala.util.Try

object ListSpec extends Properties("List") {

  val intList = Gen.listOf(Gen.choose(0, 100))
  val neIntList = intList.filter(_.nonEmpty)
  val _int = Gen.chooseNum(Int.MinValue, Int.MaxValue)
  val sameElementList = _int.flatMap(Gen.listOf(_))

  property("reverse") = forAll(intList) { (l: List[Int]) =>
    l.reverse.reverse == l
  }

  property("reverse with head") = forAll(intList) { (l: List[Int]) =>
    l.reverse.headOption == l.lastOption
  }

  property("sum is not negative") = forAll(intList) { (l: List[Int]) =>
    0 <= l.sum
  }

  property("reverse has the same sum") = forAll(intList) { (l: List[Int]) =>
    l.sum == l.reverse.sum
  }

  property("reverse has the same sum") = forAll(sameElementList) { (l: List[Int]) =>
    l.sum == l.size * l.headOption.getOrElse(0)
  }

  property("max") = forAll(sameElementList.filterNot(_.isEmpty)) { (l: List[Int]) =>
    l.max == l.head
  }

  property("max neIntLists") = forAll(neIntList, neIntList) { (l1: List[Int], l2: List[Int]) =>
    (l1.max max l2.max) == (l1 ::: l2).max
  }

  property("max of Lists") = forAll(intList, intList) { (l1: List[Int], l2: List[Int]) =>
    Try((l1 ::: l2).max).toOption == ((l1, l2) match {
      case (Nil, Nil) => None
      case (Nil, ints) => Some(ints.max)
      case (ints, Nil) => Some(ints.max)
      case (a, b) => Some((a ::: b).max)
    })
  }

  property("max neIntLists and maybe not empty list") = forAll(intList, neIntList) { (l1: List[Int], l2: List[Int]) =>
    (Try(l1.max).getOrElse(Int.MinValue) max l2.max) == (l1 ::: l2).max
  }

}
