package arb.dataStructures

sealed trait Either[+E, +A] {
	def map[B](f: A => B): Either[E, B] = this match {
		case Left(e) => Left(e)
		case Right(a) => Right(f(a))
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
		case Left(e) => Left(e)
		case Right(a) => f(a)
	}

	def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
		case Left(e) => b
		case x => x
	}

	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
		case (Right(x), Right(y)) => Right(f(x,y))
		case (Left(x) , _ ) => Left(x)
		case (_ , Left(y)) => Left(y)
	}
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]