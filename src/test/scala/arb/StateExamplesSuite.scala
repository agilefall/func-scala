package arb

import org.scalatest.FunSuite
import arb.dataStructures.State

class StateExamplesSuite extends FunSuite {

	test("list seq") {
		val l = List(1,2,3)

		val rollingAvg = l.map(i => State {
			st: (Int, Int) =>
				val st2 = (st._1 + i, st._2 + 1)
//				println(i, st, st2, (st2._1.toDouble / st2._2 , st2))
				(st2._1.toDouble / st2._2, st2)
		})
		assert(State.seq3(rollingAvg).run((0,0))._1 === List(1.0, 1.5, 2.0))
	}


}
