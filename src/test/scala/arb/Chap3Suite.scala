package arb

import org.scalatest.FunSuite
import arb.dataStructures._

import arb.exercises.Chapter3
import arb.exercises.Chapter3._
import arb.exercises.Chapter3.List._

class Chap3Suite extends FunSuite {

	test("matching ex 1") {
		val x = Chapter3.List(1, 2, 3, 4, 5) match {
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + sum(t)
			case _ => 101
		}
		assert(x == 3)
	}

	test("ex 2 tail") {
		assert(tail(List(1, 2, 3)) === List(2, 3))
		assert(tail(Nil) === Nil)
	}

	test("ex 3 drop") {
		assert(drop(List(1, 2, 3), 0) === List(1, 2, 3))
		assert(drop(List(1, 2, 3), 1) === List(2, 3))
		assert(drop(List(1, 2, 3), 2) === List(3))
	}

	test("ex 4 drop while") {
		assert(dropWhile(List(1, 2, 3))(x => x < 1) === List(1, 2, 3))
		assert(dropWhile(List(1, 2, 3))(x => x < 3) === List(3))
		assert(dropWhile(List(1, 2, 3))(_ => true) === Nil)
	}

	test("ex 5 setHead") {
		assert(setHead(List(1, 2, 3), 44) === List(44, 2, 3))
		assert(setHead(Nil, 44) === Nil)
		assert(setHead(List(1), 44) === List(44))
	}

	test("ex 6 init") {
		assert(init(Nil) === Nil)
		assert(init(List(1)) === Nil)
		assert(init(List(1, 2, 3)) === List(1, 2))
	}

	test("ex 7") {
		// can you short circuit foldRight?
		// No
		assert(true)
	}

	test("ex 8") {
		// fold right exploration
		// it no-ops the list because it processes the nil arg at the end
		assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) === List(1, 2, 3))
	}

	test("ex 9 length with foldRight") {
		assert(length(Nil) === 0)
		assert(length(List(5)) == 1)
		assert(length(List("a", "b")) == 2)
	}

	test("Ex 10 fold right causes stack overflow") {
		// this is hard to do because the apply will cause stack overflow before we can
		// run foldRight
		assert(true)
	}

	test("ex 10 fold left") {
		assert(foldLeft(List(1, 3, 4), 0)((cnt, _) => cnt + 1) === 3)
		assert(foldLeft(Nil, 0)((cnt, _) => cnt + 1) === 0)
	}

	test("ex 11 sum, prod len using foldleft") {
		assert(suml(List(1, 2, 3)) === 6)
		assert(productl(List(1, 2, 4)) === 8)
		assert(lengthl(List("a", "b", "c")) === 3)
	}
	test("ex 12 reverse") {
		assert(reverse(Nil) === Nil)
		assert(reverse(List(1)) === List(1))
		assert(reverse(List(1, 2)) === List(2, 1))
		assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
		assert(reverse(List(1, 2, 3, 4)) === List(4, 3, 2, 1))
	}

	test("ex 13") {
		// you can not write fold left in terms of fold right because of the
		// stack overflow problem
		assert(ex13foldRight(List(1, 2, 3), 0)(_ + _) === 6)
	}

	test("ex 14 append") {
		assert(append(Nil, List(1, 2, 3)) == List(1, 2, 3))
		assert(append(List(20, 10), List(1, 2, 3)) == List(20, 10, 1, 2, 3))
		assert(append(List(5, 4), Nil) == List(5, 4))
		assert(appendl(List(5, 4), Nil) == List(5, 4))
	}

	test("ex 15 flatten multiple lists") {
		assert(flatten(List(List(1,2,3), List(4,5,6), List(7,8,9))) === List(1,2,3,4,5,6,7,8,9))
		assert(flatten(List(List(1,2,3), Nil, List(7,8,9))) === List(1,2,3,7,8,9))
		assert(flatten(List(Nil, List(1,2,3), List(7,8,9))) === List(1,2,3,7,8,9))
	}

	test("ex 16 - 18 map stuff") {
		assert(inc1(List(1,2,3)) === List(2,3,4))
		assert(dToS(List(2d,2.5d)) === List("2.0", "2.5"))
	}

	test("ex 19 filter") {
		assert(filter(List(1,2,3,4,5))(_ % 2 == 0) === List(2,4))
		assert(filter(List(1,2))(_ % 2 == 0) === List(2))
		assert(filter(Nil:List[Int])(_ % 2 == 0) === Nil)
	}

	test("ex 20 flatmap") {
		assert(flatMap(List(1,2,3))((i:Int) => List(i + 1)) == List(2,3,4))
	}

	test("ex 21 filter with using flatmap") {
		assert(filterfm(List(1,2,3,4,5))(_ % 2 == 0) === List(2,4))
		assert(filterfm(List(1,2))(_ % 2 == 0) === List(2))
		assert(filterfm(Nil:List[Int])(_ % 2 == 0) === Nil)
	}

	test("ex 22 and 23 add two lists") {
		assert(addLists(List(1,2,3), List(10, 20, 30)) === List(11,22,33))
	}

	test("ex 24 has subsequence") {
		assert(hasSubsequence(List(1,2,3,4,5), List(2,3,4)))
		assert(hasSubsequence(List(1,2,3,4,5), Nil))
		assert(hasSubsequence(List(1,2,3,4,5), List(1,2,3,4,5)))
		assert(hasSubsequence(List(1,2,3,4,5), List(6)) === false)
		assert(hasSubsequence(List(1,1,2,1,1,3), List(1,1,3)))
	}

	test("ex 25 count tree") {
		assert(size(Leaf(2)) == 1)
		assert(size(Branch(Leaf(2), Leaf(3))) == 2)
		assert(size(Branch(Leaf(2), Branch(Leaf(3), Leaf(5)))) == 3)
	}

	test("ex 26 max depth tree") {
		assert(depth(Leaf(2)) == 1)
		assert(depth(Branch(Leaf(2), Leaf(3))) == 2)
		assert(depth(Branch(Branch(Leaf(2), Leaf(-1)), Branch(Leaf(3),
			Branch(Leaf(2),Branch(Leaf(1), Leaf(5)))))) === 5)
	}

	test("ex 26 map tree") {
		assert(map(Leaf(2))(_ + 2) === Leaf(4))
		assert(map(Branch(Leaf(2), Leaf(3)))(_.toString)
		  === Branch(Leaf("2"), Leaf("3")))
	}

	test("ex 29 implement tree function using fold") {
		assert(true)
	}
}

