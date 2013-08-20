package arb

import org.scalatest.FunSuite

import arb.dataStructures._
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
	}


}
