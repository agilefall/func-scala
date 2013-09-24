package arb

import arb.exercises.Chapter6.RNG
import org.scalatest.FunSuite
import arb.exercises.Chapter6._

class Chap6Suite extends FunSuite {

	class MockRNG(nums: List[Int]) extends RNG {
		def nextInt = nums match {
			case Nil => throw new Exception("ran out of mock random number")
			case x :: xs => (x, new MockRNG(xs))
		}
	}

	object MockRNG {
		def apply(nums: Int*) = new MockRNG(nums.toList)
	}

	def near(ex: Double, actual: Double) = {
		if(math.abs(ex - actual) > .001) {
			throw new RuntimeException(s"Expected $ex, actual $actual")
		}
	}

	test("ex 1: positive int") {
		val mock = MockRNG(-2, 33, Int.MinValue, -44)
		val (i, ra) = positiveInt(mock)
		assert(i === 2)
		val (j, rb) = positiveInt(ra)
		assert(j === 33)
		val (k, rc) = positiveInt(rb)
		assert(k === 44)
	}

	test("ex 2 double") {
		val mock = MockRNG(-2, 33, Int.MaxValue / 2)
		val (a, ra) = double(mock)
		near(9.31e-10, a)

		val (b, rb) = double(ra)
		near(1.53e-8, b)

		val (c, rc) = double(rb)
		near(.5, c)

	}

	test("ex 3 int double") {
		val mock = MockRNG(-2, 33)
		val ((a, b), _) = intDouble(mock)
		assert(a === -2)
		near(1.53e-8, b)
	}

	test("ex 3 double int") {
		val mock = MockRNG(33,-2)
		val ((a, b), _) = doubleInt(mock)
		near(1.53e-8, a)
		assert(b === -2)
	}

	test("ex 3 double 3") {
		val mock = MockRNG(33,-2, 33)
		val ((a, b, c), _) = double3(mock)
		near(9.31e-10, a)
		near(1.53e-8, b)
		near(9.31e-10, c)
	}

	test("ex 4 ints") {
		val mock = MockRNG(3,4,8,7)
		val (rs, ra) = ints(3)(mock)
		assert(rs === List(8, 4, 3))
	}

	test("ex 5 positive max") {
		val mock = MockRNG(-Int.MaxValue, Int.MaxValue / 2 - 1)
		val (a, ma) = positiveMax(55)(mock)
		assert(a === 55)
		val (b, mb) = positiveMax(55)(ma)
		assert(b === 28)
	}

	test("ex 6 double v2") {
		val mock = MockRNG(-2, 33, Int.MaxValue / 2)
		val (a, ra) = doubleV2(mock)
		near(9.31e-10, a)

		val (b, rb) = doubleV2(ra)
		near(1.53e-8, b)

		val (c, rc) = doubleV2(rb)
		near(.5, c)
	}

	test("ex 7 int double v2") {
		val mock = MockRNG(-2, 33)
		val ((a, b), _) = intDouble(mock)
		assert(a === -2)
		near(1.53e-8, b)
	}


	test("ex 8 ints v2") {
		val mock = MockRNG(3,4,8,7)
		val (rs, ra) = ints(3)(mock)
		assert(rs === List(8, 4, 3))
	}


	test("ex 9: positive int V2") {
		val mock = MockRNG(-2, 33, Int.MinValue, -44)
		val (i, ra) = positiveInt(mock)
		assert(i === 2)
		val (j, rb) = positiveIntV2(ra)
		assert(j === 33)
		val (k, rc) = positiveIntV2(rb)
		assert(k === 44)
	}

	test("ex 10 map v2 ") {
		def positiveMax2(n: Int): Rand[Int] = {
			map(double) {
				d => println(d); math.round((d * n).toFloat);
			}
		}

		val mock = MockRNG(-Int.MaxValue, Int.MaxValue / 2 - 1)
		val (a, ma) = positiveMax(55)(mock)
		assert(a === 55)
		val (b, mb) = positiveMax(55)(ma)
		assert(b === 28)
	}

	test("ex 10 map2 v2") {

		def intDoubleV3(rng: RNG): ((Int, Double), RNG) = {
			map2V2(int, double) {
				(i, d) => (i, d)
			}(rng)
		}

		val mock = MockRNG(-2, 33)
		val ((a, b), _) = intDoubleV3(mock)
		assert(a === -2)
		near(1.53e-8, b)
	}


	test("ex 11 candy machine") {
		assert((0,1) === simulateMachine(List(Turn)).run(Machine(false, 1, 0))._1)

		assert((0,1) === simulateMachine(List(Coin, Turn)).run(Machine(true, 1, 0))._1)
	}


}
