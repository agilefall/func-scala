package arb.dataStructures

import scala.annotation.tailrec


object State {
	import scala.collection.immutable.{List => Lst}

	def unit[S, A](a: A): State[S, A] = State((s:S) => (a,s))
//	def sequence[S, A](fs: Lst[State[S, A]]): State[S, List[A]] = {
//		@tailrec
//		def loop(l:Lst[State[S, A]], accum: State[S, Lst[A]]) = l match {
//			case Nil => accum
//			case x::xs => {
//				(s:S) => val (a, s2) = x.run(s)
//				loop(xs, State(s2, a::accum))
//			}
//		}
//	}
}

case class State[S,+A](run: S => (A,S)) {

	def flatMap[B](f: A => State[S, B]): State[S, B] = {
		State((s: S) => {
			val (a, s2) = run(s)
			f(a).run(s2)
		})
	}

	def map[B](f: A => B): State[S, B] = {
		flatMap { a =>
			State((s2) => (f(a), s2))
		}
	}

	def map2[B, C](st: State[S, B])(f: (A, B) => C): State[S,C] = {
		flatMap{ a =>
			State((s:S) => {
				val (b, s2) =  st.run(s)
				val x = (f(a,b), s2)
				x
			})
		}
	}


}