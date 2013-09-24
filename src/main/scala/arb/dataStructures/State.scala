package arb.dataStructures

import scala.annotation.tailrec


object State {

	import scala.collection.immutable.{List, Nil}

	def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

	def get[S]: State[S, S] = State(s => (s, s))

	def set[S](s: S): State[S, Unit] = State(_ => ((), s))

	def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
		unit[S, List[A]](Nil).flatMap {
			a =>
				State((s: S) => fs.foldLeft((a, s)) {
					(accum: (List[A], S), f: State[S, A]) =>
						val (a2, st2) = f.run(accum._2)
						(accum._1:::List(a2), st2)
				})
		}
	}

	def seq2[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
		State(s0 => fs.foldLeft((List[A](), s0)) {
			(accum, f) =>
				val (a, st2) = f.run(accum._2)
				(accum._1 ::: List(a), st2)
		})
	}

	def seq3[S,A](fs: List[State[S, A]]): State[S, List[A]] = {
		fs.foldRight(unit[S, List[A]](List[A]())) { (f, accum) => {
			f.map2(accum)(_ :: _)
		}}
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