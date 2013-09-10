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

	def takeWhileV1(p:(A) => Boolean): Stream[A] = this.uncons match {
		case None => Stream.empty
		case Some((v,s)) => if(!p(v)) Stream.empty else Stream.cons(v, s.takeWhileV1(p))
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B =
		uncons match {
			case Some((h, t)) => f(h, t.foldRight(z)(f))
			case None => z
		}

	def forAll(p: A => Boolean): Boolean = foldRight(true) {
		(e, a) => p(e) && a
	}

	def takeWhile(p:(A) => Boolean): Stream[A] = {
		foldRight(Stream.empty[A]){(e,a) =>
			if(p(e)) Stream.cons(e, a) else Stream.empty
		}
	}

	def map[B](f: (A) => B): Stream[B] = foldRight(Stream.empty[B]) {
		(e, a) => Stream.cons(f(e), a)
	}

	def filter(p: (A) => Boolean): Stream[A] = foldRight(Stream.empty[A]){
		(e,a) => if(p(e)) Stream.cons(e,a) else a
	}

	def append[A](a2: Stream[A]): Stream[A] = foldRight(a2) {
		(e, a) => Stream.cons(e.asInstanceOf[A], a)
	}

	def flatMap[B](f: (A) => Stream[B]): Stream[B] = foldRight(Stream.empty[B]) {
		(e, a) => f(e).append(a)
	}

	def zip[B](other: Stream[B]): Stream[(A, B)] = Stream.unfold((this, other)) {
		case (s1, s2) =>
			s1.uncons.flatMap {
				case (x, xs) => s2.uncons.map {
					case (y, ys) => ((x, y), (xs, ys))
				}
			}
	}
	def zipAll[B, C >: A](other:Stream[B]):Stream[(Option[A], Option[B])] = Stream.unfold((this, other)){
		case (s1, s2) =>
			(s1.uncons, s2.uncons) match {
				case (None, None) => None
				case (Some((x,xs)), Some((y,ys))) => Some((Some(x),Some(y)),(xs, ys))
				case (Some((x,xs)), None) => Some((Some(x), None), (xs, Stream.empty))
				case (None, Some((y,ys))) => Some((None, Some(y)), (Stream.empty, ys))
			}
	}

	def tails: Stream[Stream[A]] = Stream.unfold(this){s => s.uncons.map { case (x,xs) => (s, xs)}}.append(Stream(Stream.empty))
}

object Stream {
	import scala.{Option, None, Some}

	def empty[A]: Stream[A] =
		new Stream[A] { def uncons = None }

	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
		new Stream[A] {
			lazy val uncons = Some((hd, tl))
		}
	def apply[A](as: A*): Stream[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))


	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
		case None => Stream.empty
		case Some((a, x)) => Stream.cons(a, unfold(x)(f))
	}

	def constant[A](a: A): Stream[A] = unfold(a){_ => Some(a, a)}

	def from(n: Int): Stream[Int] = unfold(n){(s) => Option((s,s + 1))}

	def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = {
		s2.zipAll(s).foldRight(true){(a, b) =>
			(a._1 == None) || (a._1 == a._2 && b)
		}
	}
}