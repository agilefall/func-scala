package arb.exercises

import arb.dataStructures._

object Chapter4 {

	def variance(xs: Seq[Double]): Option[Double] = {
		if (xs.size == 0) None
		else {
			val m = (xs.sum / xs.size)
			Some(xs.foldLeft(0.0)((accum,x) => accum + math.pow(x - m,2))  / xs.size)
		}
	}

}
