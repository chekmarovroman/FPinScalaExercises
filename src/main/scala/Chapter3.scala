object Chapter3 extends App{

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //Exercise 2
  def tail[A](l: List[A]) = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  //Exercise 3
  def setHead[A](head: A, l: List[A]) = l match {
    case Nil => Cons(head, Nil)
    case Cons(_, t) => Cons(head, t)
  }

  //Exercise 4
  def drop[A](n: Int, l: List[A]): List[A] =
    if (n <= 0) l
    else drop(n - 1, tail(l))

  //Exercise 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  //Exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  //Exercise 9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) => y + 1)

  //Exercise 10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //Exercise 11
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)

  //Exercise 12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  //Exercise 14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  //Exercise 16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  //Exercise 17
  def doubleListToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  //Exercise 18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  //Exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => {
      if (f(h)) Cons(h, t)
      else t
    })

  //Exercise 20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((h, t) => append(f(h), t))

  //Exercise 22
  def addInt(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addInt(xs, ys))
  }

  //Exercise 23
  def zip[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zip(xs, ys)(f))
  }

  //Exercise 25
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  //Exercise 26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  //Exercise 27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  //Exercise 28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //Exercise 29
  def fold[A,B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + l + r)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(x => x)((l, r) => l.max(r))

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + l.max(r))

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

}
