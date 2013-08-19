package arb.exercises

object Chapter2 {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    (1 until as.length).forall(i => gt(as(i), as(i - 1)))
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =  (a:A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a:A, b:B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a:A)=>f(g(a))
}
