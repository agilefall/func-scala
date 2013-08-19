package arb.dataStructures

sealed trait Option[+A] {
	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(x) => Some(f(x))
	}

	def flatMap[B](f: A => Option[B]): Option[B] = {
		map(f).getOrElse(None)
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(x) => x
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = ???

	def filter(f: A => Boolean): Option[A] = flatMap((a) => {
		if (f(a)) Some(a) else None
	})

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]