package arb.dataStructures

import scala.annotation.tailrec


trait Stream[+A] {
	import scala.{List, Nil, Option, None, Some}

	def uncons: Option[(A, Stream[A])]
	def isEmpty: Boolean = uncons.isEmpty
	def toList(): List[A] = {
		@tailrec
		def loop(st: Stream[A], accum: List[A]):List[A] = st.uncons match {
			case None => accum
			case Some((v, s)) => loop(s, accum ::: List(v))
		}
		loop(this, Nil)
	}

	def take(n: Int): Stream[A] = {
		if (n == 0) Stream.empty
		else this.uncons match {
			case None => Stream.empty
			case Some((v, s)) => Stream.cons(v, s.take(n - 1))
		}
	}

	def takeWhile(p:(A) => Boolean): Stream[A] = this.uncons match {
		case None => Stream.empty
		case Some((v,s)) => if(!p(v)) Stream.empty else Stream.cons(v, s.takeWhile(p))
	}

	def forAll(p: A => Boolean): Boolean = this.uncons match {
		case None => true
		case Some((v,s)) => p(v) && s.forAll(p)
	}
}

object Stream {
	import scala.None
	import scala.Some

	def empty[A]: Stream[A] =
		new Stream[A] { def uncons = None }
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
		new Stream[A] {
			lazy val uncons = Some((hd, tl))
		}
	def apply[A](as: A*): Stream[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))
}