
object Chapter5 extends App{

  sealed trait Stream[+A] {
    import Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    //Exercise 1
    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }

    //Exercise 2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, _) if n == 1 => cons(h(), Empty)
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n == 1 => t()
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    //Exercise 3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) =>
        val hh = h()
        if (p(hh)) Stream.cons(hh, t().takeWhile(p))
        else Empty
      case _ => Empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def exists2(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    //Exercise 4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    //Exercise 5
    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((h, t) =>
        if (p(h)) cons(h, t)
        else Empty
      )

    //Exercise 6
    def headOption2: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    //Exercise 7
    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((h, t) => cons(f(h), t))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((h, t) =>
        if (p(h)) cons(h, t)
        else t
      )

    def append[B >: A](s: Stream[B]): Stream[B] =
      foldRight(s)(cons(_, _))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Empty: Stream[B])((h, t) => f(h).append(t))

    //Exercise 13
    def map2[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case Empty => None
      }

    def take2(n: Int): Stream[A] =
      unfold((this, n))(x => x._1 match {
        case Cons(h, t) if x._2 > 0 => Some((h(), (t(), x._2 - 1)))
        case _ => None
      })

    def takeWhile3(p: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
      }

    def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None: Option[B]), (t1(), Empty))
        case (Empty, Cons(h2, t2)) => Some((None: Option[A], Some(h2())), (Empty, t2()))
        case _ => None
      }

    def zip[B](s2: Stream[B]): Stream[(A,B)] =
      zipWith(s2)((_,_))

  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = cons(1, ones)

    //Exercise 8
    def constant[A](a: A): Stream[A] =
      cons(a, constant(a))

    //Exercise 9
    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

    //Exercise 10
    def fibs: Stream[Int] = {
      def innerFibs(a: Int, b: Int): Stream[Int] = {
        cons(a, innerFibs(b, a + b))
      }
      innerFibs(0, 1)
    }

    //Exercise 11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }

    //Exercise 12
    def fibs2: Stream[Int] =
      unfold((0, 1))(x => Some((x._1, (x._2, x._1 + x._2))))

    def from2(n: Int): Stream[Int] =
      unfold(n)(x => Some((x, x + 1)))

    def constant2[A](a: A): Stream[A] =
      unfold(a)(x => Some(x, x))

    val ones2: Stream[Int] =
      unfold(1)(x => Some(x, x))
  }
}
