package arb.dataStructures

import scala.annotation.tailrec


object State {

	import scala.collection.immutable.{List, Nil}

	def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

	def get[S]: State[S, S] = State(s => (s, s))

	def set[S](s: S): State[S, Unit] = State(_ => ((), s))

	def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
		@tailrec
		def loop(l: List[State[S, A]], s: S, accum: List[A]): (List[A], S) = l match {
			case Nil => (accum, s)
			case x :: xs => {
				val (a, s2) = x.run(s)
				loop(xs, s2, a::accum)
			}
		}
		State(s => loop(fs, s, Nil))
	}
}

case class State[S, +A](run: S => (A, S)) {

	def flatMap[B](f: A => State[S, B]): State[S, B] = {
		State((s: S) => {
			val (a, s2) = run(s)
			f(a).run(s2)
		})
	}

	def map[B](f: A => B): State[S, B] = {
		flatMap {
			a =>
				State((s2) => (f(a), s2))
		}
	}

	def map2[B, C](st: State[S, B])(f: (A, B) => C): State[S, C] = {
		flatMap {
			a =>
				State((s: S) => {
					val (b, s2) = st.run(s)
					(f(a, b), s2)
				})
		}
	}


}