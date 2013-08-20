package arb

import org.scalatest.FunSuite

import arb.dataStructures.{Option, Some, None}
import arb.dataStructures.{Left, Right}
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
		assert(sequence(List(Some(3), None, Some(3))) === None)
	}

	test("ex 7 either map") {
		assert(Left(2).map((x:Int) => x + 2) == Left(2))
		assert(Right(2).map((x:Int) => x + 2) == Right(4))
	}

	test("ex 7 either flatmap") {
		assert(Right(Right(3)).flatMap(r => r.flatMap(x => Right(s"xx$x"))) == Right("xx3"))
		assert(Left("left ftw").flatMap((r:Right[Int]) => r.flatMap(x => Right(s"xx$x"))) == Left("left ftw"))
	}

	test("ex 7 either or else") {
		assert(Left("abc").orElse(Right(2)) === Right(2))
		assert(Right("abc").orElse(Right(1234)) === Right("abc"))
	}

	test("ex 7 either map2") {
		assert(Right(2).map2(Right(3))(_ + _) == Right(5))
		assert(Right(2).map2(Left("xx"))((a:Int, b:Int) => 2) === Left("xx"))
		assert(Left(2).map2(Left("xx"))((a:Int, b:String) => 2.2) === Left(2))
		assert(Left(3).map2(Right("xx"))((a:Int, b:String) => List("x")) === Left(3))
	}

	test("ex 8 either sequence") {
		assert(sequence(List(Right(2), Right(3), Right(4))) === Right(List(2,3,4)))
		assert(sequence(List(Right(2), Left("xxy"), Right(4))) === Left("xxy"))
		assert(sequence(List(Left("xxy"), Right(4))) === Left("xxy"))
	}

}
