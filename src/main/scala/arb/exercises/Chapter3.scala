package arb.exercises

import arb.dataStructures._
import scala.annotation.tailrec
import arb.dataStructures.Cons

object Chapter3 {

	object List {
		def sum(ints: List[Int]): Int = ints match {
			case Nil => 0
			case Cons(x, xs) => x + sum(xs)
		}

		def product(ds: List[Double]): Double = ds match {
			case Nil => 1.0
			case Cons(0.0, _) => 0.0
			case Cons(x, xs) => x * product(xs)
		}

		def apply[A](as: A*): List[A] =
			if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))

		val example = Cons(1, Cons(2, Cons(3, Nil)))
		val example2 = List(1, 2, 3)
		val total = sum(example)

		def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}
	}

	def tail[A](l: List[A]): List[A] = l match {
		case Cons(hd, tl) => tl
		case Nil => Nil
	}

	def drop[A](l: List[A], n: Int): List[A] = n match {
		case 0 => l
		case i if i > 0 => drop(tail(l), i - 1)
		case i => throw new IllegalArgumentException(s"cannot drop $n elements")
	}

	def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(hd, tl) => if (f(hd)) dropWhile(tl)(f) else l
	}

	def init[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(hd, Nil) => Nil
		case Cons(hd, tl) => Cons(hd, init(tl))
	}

	def setHead[A](l: List[A], v:A): List[A] = l match {
		case Nil => Nil
		case Cons(hd, tl) => Cons(v, tl)
	}

	def length[A](l: List[A]): Int = {
		List.foldRight(l, 0)((_, len) => len + 1)
	}

	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
		@tailrec
		def loop(la:List[A], accum: B):B = la  match {
			case Nil => accum
			case Cons(hd, tl) => loop(tl,f(accum,hd))
		}
		loop(l, z)
	}

	def suml(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

	def productl(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

	def lengthl[A](l: List[A]): Int =  foldLeft(l, 0)((len, _) => len + 1)

	def reverse[A](l: List[A]): List[A] = {
		foldLeft(l, Nil:List[A])((acc, hd) => Cons(hd, acc))
	}

	def ex13foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
		foldLeft(reverse(l), z)((b, a) => f(a, b))
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = {
		List.foldRight(a1, a2)((a: A, l: List[A]) => {
			Cons(a, l)
		})
	}

	def appendl[A](a1: List[A], a2: List[A]): List[A] = {
		foldLeft(reverse(a1), a2)((l: List[A], a: A) => {
			Cons(a, l)
		})
	}

	def flatten[A](lists: List[List[A]]): List[A] = {
		List.foldRight(lists, Nil: List[A])((l, accum) => {
			List.foldRight(l, accum)(Cons(_, _))
		})
	}

	def map[A,B](l: List[A])(f: A => B): List[B] = l match {
		case Nil => Nil
		case Cons(hd, tl) => Cons(f(hd), map(tl)(f))
	}

	def inc1(l: List[Int]) = map(l)(_ + 1)
	def dToS(l: List[Double]) = map(l)(v => "%1.1f".format(v))

	def filter[A](list: List[A])(f:(A)=>Boolean):List[A] = {
		List.foldRight(list, Nil:List[A])((a, accum) => {
			if(f(a)) Cons(a, accum)
			else accum
		})
	}

	def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
		flatten(map(l)(f))
	}

	def filterfm[A](list: List[A])(f:(A)=>Boolean):List[A] = {
		flatten(map(list)((a:A) => if(f(a)) List(a) else Nil))
	}

	def merge[A](a:List[A], b:List[A])(f:(A, A) => A): List[A] = (a, b) match {
		case (Nil, Nil) => Nil
		case (Cons(hda, tla), Cons(hdb, tlb)) => Cons(f(hda, hdb), merge(tla, tlb)(f))
		case _ => throw new RuntimeException("Lists must be same length")
	}


	def addLists(a:List[Int], b:List[Int]): List[Int] = merge(a,b)(_ + _)

	def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
		def isSub(a:List[A], s: List[A]): Boolean = a match {
			case Nil => s == Nil
			case Cons(hd, tl) => s match {
				case Nil => true
				case Cons(hdb, tlb) => if (hdb != hd) false else isSub(tl, tlb)
			}
		}
		l match {
			case Nil => sub == Nil
			case Cons(hd, tl) => isSub(l, sub) || hasSubsequence(tl, sub)
		}
	}

	def fold[A, B](t:Tree[A])(f: (A) => B)(g: (B, B) => B): B = t match {
		case Leaf(x) => f(x)
		case Branch(a, b) => g(fold(a)(f)(g), fold(b)(f)(g))
	}

	def size(t: Tree[_]): Int = fold(t)(_  => 1)(_ + _)

	def depth(t: Tree[_]): Int = fold(t)(_ => 1)(1 + Math.max(_, _))

	def map[A, B](t: Tree[A])(f: (A) => B): Tree[B] = {
		fold[A, Tree[B]](t)((a: A) => Leaf[B](f(a)))((a: Tree[B], b: Tree[B]) => Branch[B](a, b))
	}

}