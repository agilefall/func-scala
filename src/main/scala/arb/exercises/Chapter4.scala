package arb.exercises

import arb.dataStructures.{Option, Some, None, Right, Left, Either}
import scala.annotation.tailrec

object Chapter4 {

	def variance(xs: Seq[Double]): Option[Double] = {
		if (xs.size == 0) None
		else {
			val m = xs.sum / xs.size
			Some(xs.foldLeft(0.0)((accum,x) => accum + math.pow(x - m,2))  / xs.size)
		}
	}

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		a.flatMap(x => b.map(y => f(x,y)))
	}

	import java.util.regex._
	def pattern(s: String): Option[Pattern] =
		try {
			Some(Pattern.compile(s))
		} catch {
			case e: PatternSyntaxException => None
		}

	def mkMatcher(pat: String): Option[String => Boolean] =
		pattern(pat) map (p => (s: String) => p.matcher(s).matches)

	def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
		map2(mkMatcher(pat), mkMatcher(pat2))((m1, m2) => m1(s) && m2(s))

	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
		def lp(l: List[A], accum: List[B]): Option[List[B]] = l match {
			case Nil => Some(accum)
			case x::xs => f(x).flatMap(a => lp(xs, accum:::List(a)))
		}
		lp(a, Nil)
	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] = {
		traverse(a)(identity)
	}


	def sequence[E, B](a: List[Either[E, B]]): Either[E, List[B]] = {
		@tailrec
		def loop(a: List[Either[E, B]], accum: List[B]): Either[E, List[B]] = a match {
			case Nil => Right(accum)
			case hd :: tl => hd match {
				case Left(x) => Left(x)
				case Right(x) => loop(tl, accum ::: List(x))
			}
		}
		loop(a, Nil)
	}

}
