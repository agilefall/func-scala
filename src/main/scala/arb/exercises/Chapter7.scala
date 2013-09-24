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
		def unit[A](a: A) = (ex: ExecutorService) => {
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

		// ex 4
		def asyncF[A,B](f: A => B): A => Par[B] = {
			(a:A) => map2(unit(a), unit(Unit)){(a, _) => f(a)}
		}

		def run[A](ex: ExecutorService)(a: Par[A]): A = {
			a(ex).get(1000, TimeUnit.MILLISECONDS)
		}

	}
}
