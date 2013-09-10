package arb.exercises

import arb.dataStructures.Stream

object Chapter5 {

	def fromV1(n: Int): Stream[Int] = Stream.cons(n, fromV1(n + 1))

	def fibsV1: Stream[Int] = {
		def f(i: Int, j: Int): Stream[Int] = {
			val t = i + j
			Stream.cons(i, f(j, t))
		}
		f(0, 1)
	}

	def fibs: Stream[Int] = {
		Stream.unfold[Int, (Int, Int)]((0, 1)){(p) => Some(p._1, (p._2, p._1 + p._2))}
	}

	def constantV1[A](a: A): Stream[A] = Stream.cons(a, constantV1(a))

	val ones = Stream.unfold(1)(s => Some(1,1))
}
