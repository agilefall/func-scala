package arb

import org.scalatest.FunSuite

import arb.dataStructures.{Option, Some, None}
import arb.exercises.Chapter4._


class Chap4Suite extends FunSuite {

	test("ex 1 option test") {
		assert(Some(2).map(_ + 2) === Some(4))
		assert(None.map((x: Int) => x + 2) === None)

		assert(Some(3).filter(_ > 2) === Some(3))
		assert(Some(1).filter(_ > 2) === None)
		assert(None.filter(_ => true) === None)

		assert(Some("x").flatMap(x => Some(x + "yy")) === Some("xyy"))
		assert(None.flatMap(x => Some(x.toString + "yy")) === None)

		assert(Some("x").getOrElse("y") === "x")
		assert(None.getOrElse("y") === "y")

		assert(Some("x").orElse(Some("y")) === Some("x"))
		assert(None.orElse(Some("y")) === Some("y"))
		assert(None.orElse(None) === None)

	}

	test("Ex 2 variance") {
		assert(math.abs(variance(Seq(1,3,3,4,5,5,6,22,33)).getOrElse(throw new RuntimeException("xx")) - 105.20) < .01)
		assert(variance(Seq()) === None)
	}

	test("ex 3 map2") {
		assert(map2(Some(2), Some("x"))((i:Int, s:String) => s + i.toString) === Some("x2"))
		assert(map2(None, Some("x"))((i:Int, s:String) => s + i.toString) === None)
		assert(map2(Some(2), None)((i:Int, s:String) => s + i.toString) === None)
	}

	test("ex 4 bothMatch") {
		assert(bothMatch("^123.*", ".*333$", "1234333") === Some(true))
		assert(bothMatch("""^\y123*""", "][[]](((", "1234333") === None)
	}

	test("ex 5/6 sequence and traverse") {
		assert(sequence(List(Some(1), Some(2))) === Some(List(1,2)))
		assert(sequence(List(Some(1), None)) === None)
		assert(sequence(List(Some(()=>2), None, Some(()=> throw new RuntimeException("should short circuit")))) === None)
	}


}
