object Chapter4 extends App{

  sealed trait Option[+A]{

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    if (map(f) getOrElse false) this
    else None
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  //Exercise 1
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //Exercise 2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  //Exercise 3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  //Exercise 5
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(a: List[Option[A]], acc: List[A]): Option[List[A]] = a match {
      case Nil => Some(acc)
      case Some(x) :: xs => go(xs, acc :+ x)
      case None :: xs => None
    }
    go(a, List[A]())
  }

  //Exercise 6
  sealed trait Either[+E, +A] {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case x :: xs => f(x) flatMap (xx => traverse(xs)(f) map (xx :: _))
    }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(x => x)

    //Exercise 7
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(x) => Right(f(x))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = map(f) match {
      case Left(e) => Left(e)
      case Right(x) => x
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(_) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this flatMap (aa => b map (bb => f(aa, bb)))
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  //Exercise 8
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case x :: xs => f(x) flatMap(xx => traverse(xs)(f) map (xx :: _))
    }

}
