package arb

import org.scalatest.FunSuite
import arb.exercises.Chapter2
import Chapter2._

class Chap2Suite extends FunSuite {
  val gt = (x: Int, y: Int) => x > y

  test("is sorted with sorted elements") {
    assert(isSorted(Array(1, 2, 3, 4), gt))
  }

  test("is not sorted last element off") {
    assert(!isSorted(Array(1, 2, 3, 2), gt))
  }

  test("is not sorted first element off") {
    assert(!isSorted(Array(5, 2, 3, 7), gt))
  }

  test("sorted empty list") {
    assert(isSorted(Array[Int](), gt))
  }

  test("partial func") {
    assert(partial1(3, (a:Int,b:Int)=> a < b)(4))
    assert(partial1("dog", (a:String,b:String)=> (a+b).length)("cat") == 6)
  }

  test("curry") {
    assert(curry((a:Int, b:String)=> b.length > a)(2)("dog"))
  }

}
