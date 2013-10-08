package arb.exercises

import java.util.concurrent.{TimeUnit, Callable, Future, ExecutorService}

object Chapter7 {
	// exercise v1
	object ParV1 {
		def unit[A](a: => A) = ParV1(a)
		def get[A](p: ParV1[A]) = p.a

		def map2[A, B, C](a: ParV1[A], b: ParV1[B])(f:(A, B) => C): ParV1[C] = {
			unit(f(ParV1.get(a), ParV1.get(b)))
		}

	}
	case class ParV1[A](a: A)

	// exercise 2
	// the executor service can return
	type Par[A] = ExecutorService => Future[A]

	object Par {
		def unit[A](a: A):Par[A] = (ex: ExecutorService) => {
			ex.submit(new Callable[A] {
				def call() = a
			})
		}

		def map2[A, B, C](a: Par[A], b:Par[B])(f: (A,B) => C): Par[C] = (ex: ExecutorService) => {
			ex.submit(new Callable[C] {
				def call() = {
					val aa = a(ex)
					val bb = b(ex)
					f(aa.get(),bb.get())
				}
			})
		}

		def async[A](a: => A): Par[A] = fork(unit(a))

		//todo: is this right?
		def fork[A](a: => Par[A]): Par[A] = (ex: ExecutorService) => {
			ex.submit(new Callable[A]{
				def call() = a(ex).get()
			})
		}

		def run[A](ex: ExecutorService)(a: Par[A]): A = {
			a(ex).get(1000, TimeUnit.MILLISECONDS)
		}

		// ex 4
		def asyncF[A,B](f: A => B): A => Par[B] = {
			(a:A) => map2(unit(a), unit(Unit)){(a, _) => f(a)}
		}

		def map[A,B](fa: Par[A])(f: A => B): Par[B] = {
			map2(fa, unit(()))((a,_) => f(a))
		}

		// ex 5
		def sequence[A](l: List[Par[A]]): Par[List[A]] = // (es: ExecutorService) => {
//			es.submit(new Callable[List[A]]{
//				def call() = {
//					l.map(a => a(es)).map(_.get)
//				}
//			})
			l.foldRight(unit(List[A]())) {
				(a, accum) => {
					map2(a, accum)(_ :: _)
				}
			}

		def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = fork {
			val fbs: List[Par[B]] = l.map(asyncF(f))
			sequence(fbs)
		}

		// ex 6
		def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
		  	map(parMap(l)(a => if(f(a)) Some(a) else None))(_.flatten)
		}

		//ex 11
		def choiceV1[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = (es: ExecutorService) => {
			if (run(es)(a)) ifTrue(es) else ifFalse(es)
		}

		// ex 12
		def choiceNV1[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
			choices(run(es)(a))(es)
		}

		def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
			choiceNV1(map(a){b => if(b) 0 else 1})(List(ifTrue, ifFalse))
		}

		// ex 13 cause we always need a flatmap
		// ex 14 "chooser" is flatmap
		def flatMap[A, B](a: Par[A])(f: (A) => Par[B]): Par[B] = es => {
			f(run(es)(a))(es)
		}

		def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] = {
			flatMap(a) { choices( _ ) }
		}

		def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =  {
			flatMap[Int, A](a)(choices(_))
		}

		def choiceV2[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
			flatMap(a)(b => if(b) ifTrue else ifFalse)
		}

		// ex 15
		def join[A](a: Par[Par[A]]): Par[A] = (ex) => {
			run(ex)(a)(ex)
		}

		def flatMapJoin[A, B](a: Par[A])(f: (A) => Par[B]): Par[B] = {
			join(map(a)(f))
		}


		def joinFlatMap[A](a: Par[Par[A]]): Par[A] = {
			flatMap(a)(identity)
		}
	}

}
