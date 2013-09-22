package arb.exercises

import scala.annotation.tailrec

object Chapter6 {

	trait RNG {
		def nextInt: (Int, RNG)
	}

	object RNG {
		def simple(seed: Long): RNG = new RNG {
			def nextInt = {
				val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
				  ((1L << 48) - 1)
				((seed2 >>> 16).asInstanceOf[Int],
				  simple(seed2))
			}
		}
	}


	//ex 1
	def positiveInt(rng: RNG): (Int, RNG) = {
		val (i, nextRng) = rng.nextInt
		if (i != Int.MinValue) (math.abs(i), nextRng) else positiveInt(nextRng)
	}

	//ex 2
	def double(rng: RNG): (Double, RNG) = {
		val(i, next) = positiveInt(rng)
		(i.toDouble / Int.MaxValue, next)
	}

	// this wasn't an exercise.  I couldn't write next few functions without it
	def compose[A, B](f:(RNG) => (A,RNG), g:(RNG) => (B,RNG)): (RNG) => ((A, B), RNG)  = {
		(rng: RNG) => {
			val (a, rng1) = f(rng)
			val (b, rng2) = g(rng1)
			((a, b), rng2)
		}
	}

	// ex 3
	def intDouble(rng: RNG): ((Int,Double), RNG) = {
		compose(_.nextInt, double)(rng)
	}

	// ex 3
	def doubleInt(rng: RNG): ((Double,Int), RNG) = {
		compose(double, _.nextInt)(rng)
	}

	// ex 3
	def double3(rng: RNG): ((Double,Double,Double), RNG) = {
		val (d,rng2) = compose(compose(double,double), double)(rng)
		((d._1._1, d._1._2, d._2), rng2)
	}

	// ex 4
	def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
		@tailrec
		def loop(i: Int, rnds: List[Int], rng:RNG):(List[Int], RNG) = i match {
			case j if j <= 0 => (rnds, rng)
			case _ => {
				val (r, rnd2) = rng.nextInt
				loop(i - 1, r :: rnds, rnd2)
			}
		}
		loop(count, Nil, rng)
	}

	type Rand[+A] = RNG => (A, RNG)

	val int: Rand[Int] = _.nextInt

	def unit[A](a: A): Rand[A] =
		rng => (a, rng)

	def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
		rng => {
			val (a, rng2) = s(rng)
			(f(a), rng2)
		}

	//ex 5
	def positiveMax(n: Int): Rand[Int] = {
		map(double){d => println(d); math.round((d * n).toFloat);}
	}

	//ex 6
	def doubleV2(rng: RNG): (Double, RNG) = {
		map(int) { i => i.toDouble / Int.MaxValue }(rng)
	}

	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		map(compose(ra, rb)){p => f(p._1,p._2)}
	}

	//ex 7 int double v2
	def intDoubleV2(rng: RNG): ((Int,Double), RNG) = {
		map2(int, double){(i,d) => (i,d)}(rng)
	}

}