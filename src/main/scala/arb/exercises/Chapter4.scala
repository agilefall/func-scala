package arb.exercises

import arb.dataStructures.{Option, Some, None}

object Chapter4 {

	def variance(xs: Seq[Double]): Option[Double] = {
		if (xs.size == 0) None
		else {
			val m = (xs.sum / xs.size)
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
			case hd::tl => f(hd).flatMap(a => lp(tl, accum:::List(a)))
		}
		lp(a, Nil)
	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] = {
		traverse(a)(x => x)
	}


}
