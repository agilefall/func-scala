package arb

import org.scalatest.FunSuite


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
		assert(Stream("foo", "bar", "fizbuzz").takeWhile(_.endsWith("x")).toList() === Nil)
		assert(Stream("foo", "bar", "fizbuzz").takeWhile(_.startsWith("f")).toList() === List("foo"))
		assert(Stream("foo", "bar", "fizbuzz").takeWhile(_.size == 3).toList() === List("foo", "bar"))
		assert(Stream("foo", "bar", "fizbuzz").takeWhile(_ => true).toList() === List("foo", "bar", "fizbuzz"))
		assert(Stream.cons(1, Stream.cons(2, Stream.cons({throw new DummyException}, Stream.empty))).takeWhile(_ < 2).toList() === List(1))

		// should get no error until the new stream is used
		Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).takeWhile(_ < 3)

		intercept[DummyException] {
			Stream.cons(1, Stream.cons({throw new DummyException}, Stream.empty)).takeWhile(_ < 3).toList()
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


}