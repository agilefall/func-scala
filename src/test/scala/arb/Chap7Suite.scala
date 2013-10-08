package arb

import org.scalatest.FunSuite
import arb.exercises.Chapter7.{ParV1, Par}
import java.util.concurrent._

class Chap7Suite extends FunSuite {
	test("Ex 1  ParV1") {
		def sum(as: IndexedSeq[Int]): ParV1[Int] =
			if (as.size <= 1) ParV1.unit(as.headOption getOrElse 0)
			else {
				val (l, r) = as.splitAt(as.length / 2)
				ParV1.map2(sum(l), sum(r))(_ + _)
			}
		assert(ParV1.get(sum(Vector(1, 2, 3))) === 6)
	}

	val ec = new ThreadPoolExecutor(20, 20, 20, TimeUnit.SECONDS, new ArrayBlockingQueue(20))

	def assertTime[A](sec: Long, a: => Par[A]): A = {
		val start = System.currentTimeMillis
		val r = a(ec).get()
		val end = System.currentTimeMillis
		assert((end - start) < (sec * 1000), "took %s secs".format((end - start) / 1000))
		r
	}


	def slowIdentity[A](a: A, delayMS: Int = 500) = (ex: ExecutorService) => {
		ex.submit(new Callable[A] {
			def call() = {
				Thread.sleep(900)
				a
			}
		})
	}

	test("ex test slow identity") {
		assertTime(2, slowIdentity(3))
	}

	test("map 2 with unit") {
		assert(5 === Par.run(ec)(Par.map2(Par.unit(2), Par.unit(3))((i: Int, j: Int) => i + j)))
	}

	test("ex 2 map 2 runs in parallel") {
		val p = Par.map2(slowIdentity(3), slowIdentity(2)) {
			(a, b) => a + b
		}
		assert(5 === assertTime(2, p))
	}

	test("ex 5 sequence in parallel") {
		val l = Par.sequence(List.fill(10)(slowIdentity(5)))
		assert(List.fill(10)(5) === assertTime(2, l))
	}

	test("ex 6 filter in parallel") {
		val startTime = System.currentTimeMillis()
		val l = (1 to 15).toList
		val p = (i: Int) => { Thread.sleep(1000); i % 2 == 0}
		val fl = Par.parFilter(l)(p)(ec).get()
		val endTime = System.currentTimeMillis()

		assert(endTime - startTime < 5000, s"took ${endTime - startTime} ms" )
		assert(fl == (2 to 15).by(2).toList, fl)
	}


}
