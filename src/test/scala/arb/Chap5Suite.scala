package arb

import org.scalatest.FunSuite
import arb.exercises.Chapter5._


class Chap5Suite extends FunSuite {
	import dataStructures.Stream

	class DummyException extends RuntimeException
	test("ex 1 stream to list") {
		assert(Stream("foo", "bar", "fizbuzz").toList() === List("foo", "bar", "fizbuzz"))
		assert(Stream("foo").toList() === List("foo"))
		assert(Stream[Int]().toList() === List[Int]())
	}

	test("ex 2 stream take") {
		assert(Stream("foo", "bar", "fizbuzz").take(0).toList() === Nil)
		assert(Stream("foo", "bar", "fizbuzz").take(1).toList() === List("foo"))
		assert(Stream("foo", "bar", "fizbuzz").take(2).toList() === List("foo", "bar"))
		assert(Stream("foo", "bar", "fizbuzz").take(3).toList() === List("foo", "bar", "fizbuzz"))
		assert(Stream("foo", "bar", "fizbuzz").take(4).toList() === List("foo", "bar", "fizbuzz"))
		assert(Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).take(1).toList() === List(1))
		// should get no error until the new stream is used
		Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).take(2)
		intercept[DummyException] {
			Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).take(2).toList()
		}
	}

	test("ex 3 stream take while") {
		assert(Stream("foo", "bar", "fizbuzz").takeWhileV1(_.endsWith("x")).toList() === Nil)
		assert(Stream("foo", "bar", "fizbuzz").takeWhileV1(_.startsWith("f")).toList() === List("foo"))
		assert(Stream("foo", "bar", "fizbuzz").takeWhileV1(_.size == 3).toList() === List("foo", "bar"))
		assert(Stream("foo", "bar", "fizbuzz").takeWhileV1(_ => true).toList() === List("foo", "bar", "fizbuzz"))
		assert(Stream.cons(1, Stream.cons(2, Stream.cons({throw new DummyException}, Stream.empty))).takeWhileV1(_ < 2).toList() === List(1))

		// should get no error until the new stream is used
		Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).takeWhileV1(_ < 3)

		intercept[DummyException] {
			Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).takeWhileV1(_ < 3).toList()
		}
	}

	test("ex 4 stream forAll") {
		assert(Stream("foo", "bar", "fizbuzz").forAll(_.endsWith("x")) === false)
		assert(Stream("foo", "bar", "fizbuzz").forAll(_.startsWith("f")) === false)
		assert(Stream("foo", "bar", "fizbuzz").forAll(_.size > 2) === true)
		assert(Stream("foo", "bar", "fizbuzz").forAll(_ => true)=== true)
		assert(Stream.cons(1, Stream.cons(2, Stream.cons({throw new DummyException}, Stream.empty))).forAll(_ < 2) === false)
		intercept[DummyException] {
			Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).forAll(_ < 3)
		}
	}

	test("ex 5 stream take while using foldr") {
		assert(Stream("foo", "bar", "fizbuzz").takeWhile(_.endsWith("x")).toList() === Nil)
		assert(Stream("foo", "bar", "fizbuzz").takeWhile(_.startsWith("f")).toList() === List("foo"))
		assert(Stream("foo", "bar", "fizbuzz").takeWhile(_.size == 3).toList() === List("foo", "bar"))
		assert(Stream("foo", "bar", "fizbuzz").takeWhile(_ => true).toList() === List("foo", "bar", "fizbuzz"))
		assert(Stream.cons(1, Stream.cons(2, Stream.cons({throw new DummyException}, Stream.empty))).takeWhileV1(_ < 2).toList() === List(1))

		// should get no error until the new stream is used
		Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).takeWhile(_ < 3)

		intercept[DummyException] {
			Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).takeWhile(_ < 3).toList()
		}
	}

	test("ex 6 map using foldr") {
		assert(Stream.empty.map((x:String) =>  s"$x-x").toList() === Nil)
		assert(Stream("foo", "bar", "fizbuzz").map(x => s"$x-x").toList() === List("foo-x", "bar-x", "fizbuzz-x"))
		assert(Stream("foo").map(x => x.size).toList() === List(3))
	}

	test("ex 6 filter using foldr") {
		assert(Stream.empty.filter((x: String) => true).toList() === Nil)
		assert(Stream("foo", "bar", "fizbuzz").filter(x => x.startsWith("f")).toList() === List("foo", "fizbuzz"))
		assert(Stream("foo").filter(x => x.size == 4).toList() === Nil)
	}

	test("ex 6 append using foldr") {
		assert(Stream.empty.append(Stream.empty).toList() === Nil)
		assert(Stream("foo", "bar").append(Stream.empty).toList() === List("foo", "bar"))
		assert(Stream.empty.append(Stream("a", "b")).toList() === List("a", "b"))
		assert(Stream("a").append(Stream("x", "y")).toList() === List("a", "x", "y"))
		assert(Stream("a", "b", "c").append(Stream("x", "y", "z")).toList() === List("a", "b", "c", "x", "y", "z"))
	}

	test("ex 6 flatmap using foldr") {
		assert(Stream.empty.flatMap((x:String) => Stream(x, x)).toList() === Nil)
		assert(Stream("x").flatMap(x => Stream(x)).toList() === List("x"))
		assert(Stream("x").flatMap(x => Stream(x,x)).toList() === List("x", "x"))
		assert(Stream("x", "y").flatMap(x => Stream(x, x)).toList() === List("x", "x", "y", "y"))
		assert(Stream("x", "y", "z").flatMap(x => if(x != "y") Stream(x, x) else Stream.empty).toList() === List("x", "x", "z", "z"))
	}

	test("ex 7 constant NOT using foldr") {
		assert(constantV1("a").take(4).toList() === List("a", "a", "a", "a"))
		assert(constantV1(1).takeWhile( _ != 1).toList() === Nil)
	}

	test("ex 8 from v1") {
		assert(fromV1(2).take(3).toList().sum === 9)
		assert(fromV1(2).take(100).toList().size === 100)
	}

	test("ex 9 fibs") {
		assert(fibsV1.take(7).toList() === List(0, 1, 1, 2, 3, 5, 8))
	}

	test("ex 10 unfold") {
		assert(Stream.unfold[Int, Int](0)(x => Some(x, x + 1)).take(3).toList() === List(0,1,2))
	}

	test("ex 11 fibs with unfold") {
		assert(fibs.take(7).toList() === List(0, 1, 1, 2, 3, 5, 8))
	}

	test("ex 11 const with unfold") {
		assert(Stream.constant("a").take(4).toList() === List("a", "a", "a", "a"))
		assert(Stream.constant(1).takeWhile( _ != 1).toList() === Nil)
	}

	test("ex 11 from") {
		assert(Stream.from(2).take(3).toList().sum === 9)
		assert(Stream.from(2).take(100).toList().size === 100)
	}

	test("ex 11 ones") {
		assert(ones.take(5).toList.sum === 5)
	}

	test("ex 12 map using unfold") {
		assert(Stream.empty.map((x:String) =>  s"$x-x").toList() === Nil)
		assert(map(Stream("foo","bar", "fizbuzz"))(x => s"$x-x").toList() === List("foo-x", "bar-x", "fizbuzz-x"))
		assert(map(Stream("foo"))(x => x.size).toList() === List(3))
	}


	test("ex 12 stream take using unfold") {
		assert(take(Stream("foo", "bar", "fizbuzz"),0).toList() === Nil)
		assert(take(Stream("foo", "bar", "fizbuzz"),1).toList() === List("foo"))
		assert(take(Stream("foo", "bar", "fizbuzz", "xxyy"),2).toList() === List("foo", "bar"))
		assert(take(Stream("foo", "bar", "fizbuzz"),3).toList() === List("foo", "bar", "fizbuzz"))
		assert(take(Stream("foo", "bar", "fizbuzz"),4).toList() === List("foo", "bar", "fizbuzz"))
		assert(take(Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)),1).toList() === List(1))
		// should get no error until the new stream is used
		take(Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)),2)
		intercept[DummyException] {
			take(Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)),2).toList()
		}
	}

	test("ex 12 stream takeWhile using unfold") {
		assert(takeWhile(Stream("foo", "bar", "fizbuzz"))(_.endsWith("x")).toList() === Nil)
		assert(takeWhile(Stream("foo", "bar", "fizbuzz"))(_.startsWith("f")).toList() === List("foo"))
		assert(takeWhile(Stream("foo", "bar", "fizbuzz"))(_.size == 3).toList() === List("foo", "bar"))
		assert(takeWhile(Stream("foo", "bar", "fizbuzz"))(_ => true).toList() === List("foo", "bar", "fizbuzz"))
		assert(takeWhile(Stream.cons(1, Stream.cons(2, Stream.cons({throw new DummyException}, Stream.empty))))(_ < 2).toList() === List(1))

		// should get no error until the new stream is used
		takeWhile(Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)))(_ < 3)

		intercept[DummyException] {
			takeWhile(Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)))(_ < 3).toList()
		}
	}

	test("ex 12 zip") {
		assert(Stream(1,2,3).zip(Stream("a", "b", "c")).toList() === List((1,"a"), (2, "b"), (3, "c")))
		assert(Stream(1,2,3).zip(Stream("a", "b")).toList() === List((1,"a"), (2, "b")))
		assert(Stream.empty[Int].zip(Stream("a", "b")).toList() === Nil)
		assert(Stream.cons(1, Stream.cons(2, Stream.cons({throw new DummyException}, Stream.empty))).
		  zip(Stream("a")).toList() == List((1,"a")))
	}

	test("ex 12 zip all") {
		assert(Stream(1,2,3).zipAll(Stream("a", "b", "c")).toList() === List((Some(1),Some("a")), (Some(2), Some("b")), (Some(3), Some("c"))))
		assert(Stream(1,2,3).zipAll(Stream("a", "b")).toList() === List((Some(1),Some("a")), (Some(2), Some("b")), (Some(3), None)))
		assert(Stream(1,2).zipAll(Stream("a", "b", "c")).toList() === List((Some(1),Some("a")), (Some(2), Some("b")), (None, Some("c"))))
	}

	test("ex 13 starts with") {
		assert(Stream.startsWith(Stream(1,2,3),Stream(1,2)))
		assert(Stream.startsWith(Stream(1,2,3),Stream(1,2,3)))
		assert(Stream.startsWith(Stream(1,2,3),Stream.empty))
		assert(Stream.startsWith(Stream.empty,Stream.empty))
		assert(Stream.startsWith(Stream.empty,Stream(1)) === false)
		assert(Stream.startsWith(Stream(1,2,3),Stream(1,3)) === false)
		assert(Stream.startsWith(Stream(1,2,3),Stream(0,1)) === false)
	}

	test("ex 14 tails") {
		assert(Stream(1,2,3).tails.toList().map(_.toList) === List(List(1,2,3), List(2,3), List(3), Nil))
	}

}