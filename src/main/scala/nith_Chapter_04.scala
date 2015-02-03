
object Ch04_Option {

  sealed trait Option[+A] {
    // 4.1 Implements Option functions, better use getOrElse and map
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

    def orElse[B >: A](ob: => Option[B]): Option[B] = this.map[Option[A]](a => Some(a)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = if (this.map(f).getOrElse(false)) this else None
  }

  // QUESTION: What does "get" mean here ?
  // get is just a name for the option's element
  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  // 4.2 Implement the variance function in terms of flatMap.
  // If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(a => mean(xs.map(x => math.pow(x - a, 2))))


  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  // 4.3 Write a generic function map2 that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too. Here is its signature:
  def mapFor[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    x <- a
    y <- b
  } yield f(x, y)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)))


  // 4.4 Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list. If the original list contains None even
  // once, the result of the function should be None; otherwise the result should be Some with a
  // list of all the values. Here is its signature:
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Cons(None, t) => None
    case Cons(Some(h), t) => map2[A, List[A], List[A]](Some(h), sequence(t))((h, t) => Cons(h, t))
  }


  /*
    def parseInts(a: List[String]): Option[List[Int]] =
      sequence(a map (i => Try(i.toInt)))
  */


  // 4.5 Implement traverse. It’s straightforward to do using map and sequence, but try for a more efficient
  // implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case Cons(h, t) => f(h) match {
      case None => None
      case Some(b) => map2[B, List[B], List[B]](Some(b), traverse(t)(f))((h, t) => Cons(h, t))
    }
  }

}

object Ch04_Either {

  sealed trait Either[+E, +A] {
    // 4.6 Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this.flatMap(a => Right(a)) match {
      case Left(e) => b
      case Right(bb) => Right(bb)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(x => b.map(y => f(x, y)))
  }


  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = if (xs.isEmpty)
    Left("mean of empty list!")
  else
    Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }


  // 4.7 Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case Cons(Left(e), t) => Left(e)
    case Cons(h, t) => h.map2[E,List[A],List[A]](sequence(t))((head, tl) => Cons(head, tl))
  }
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case Cons(h, t) => f(h) match {
      case Left(e) => Left(e)
      case Right(b) => Right(b).map2[E, List[B], List[B]](traverse(t)(f))((head, tl) => Cons(head, tl))
    }
  }

  case class Person(name: Name, age: Age)

  sealed class Name(val value: String)

  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.") else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.") else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))


  // 4.8 In this implementation, map2 is only able to report one error, even if both the name and the age are invalid.
  // What would you need to change in order to report both errors? Would you change map2 or the signature of mkPerson?
  // Or could you create a new data type that captures this requirement better than Either does, with some additional
  // structure? How would orElse, traverse, and sequence behave differently for that data type?


}

object nith_Chapter_04 extends App {

  val stringLength: String => Int = s => s.length
  val evenStringLength: String => Boolean = s => s.length % 2 == 0
  // sequences
  val emptySeq: Seq[Double] = Seq()
  val singleSeq: Seq[Double] = Seq(42)
  val fiveSeq: Seq[Double] = Seq(0, 1, 2, 3, 4)
  // Option related constants
  val optionalStringLength: String => Ch04_Option.Option[Int] = s => Ch04_Option.Some(s.length)
  val optionalToInt: String => Ch04_Option.Option[Int] = x => Ch04_Option.Try {x.toInt}
  val stringIterator: (String, Int) => String = (s, i) => if (i < 1) "" else s + stringIterator(s, i - 1)
  // Either related constants
  val except: String = "Let this exception been thrown at you!"
  val eithernalStringLength: String => Ch04_Either.Either[String,Int] = s => Ch04_Either.Right(s.length)
  val eithernalToInt: String => Ch04_Either.Either[String,Int] = x => try Ch04_Either.Right(x.toInt) catch { case e: Exception => Ch04_Either.Left(except) }


  println("************************")
  println("****** Chapter_04 ******")
  println("************************")

  println("** Exercise 4.1 **")
  // map
  println("None.map(stringLength) = " + Ch04_Option.None.map(stringLength))
  println("Some(\"\").map(stringLength) = " + Ch04_Option.Some("").map(stringLength))
  println("Some(\"abc\").map(stringLength) = " + Ch04_Option.Some("abc").map(stringLength))
  // getOrElse
  println("None.getOrElse(\"\") = " + Ch04_Option.None.getOrElse(""))
  println("None.getOrElse(None) = " + Ch04_Option.None.getOrElse(None))
  println("Some(\"\").getOrElse(\"\") = " + Ch04_Option.Some("").getOrElse(""))
  println("Some(\"abc\").getOrElse(\"\") = " + Ch04_Option.Some("abc").getOrElse(""))
  println("Some(Some(\"abc\")).getOrElse(\"\") = " + Ch04_Option.Some(Ch04_Option.Some("abc")).getOrElse(""))
  // flatMap
  println("None.flatMap(optionalStringLength) = " + Ch04_Option.None.flatMap[Int](optionalStringLength))
  println("Some(\"\").flatMap(optionalStringLength) = " + Ch04_Option.Some("").flatMap(optionalStringLength))
  println("Some(\"abc\").flatMap(optionalStringLength) = " + Ch04_Option.Some("abc").flatMap(optionalStringLength))
  // orElse
  println("None.orElse(None) = " + Ch04_Option.None.orElse(Ch04_Option.None))
  println("None.orElse(Some(1)) = " + Ch04_Option.None.orElse(Ch04_Option.Some(1.0)))
  println("None.orElse(Some(2.4)) = " + Ch04_Option.None.orElse(Ch04_Option.Some(2.4)))
  println("Some(42).orElse(Some(2.4)) = " + Ch04_Option.Some(42).orElse(Ch04_Option.Some(2.4)))
  println("Some(Some(42)).orElse(Some(2.4)) = " + Ch04_Option.Some(Ch04_Option.Some(42)).orElse(Ch04_Option.Some(2.4)))
  // filter
  println("None.filter(evenStringLength) = " + Ch04_Option.None.filter(evenStringLength))
  println("Some(\"\").filter(evenStringLength) = " + Ch04_Option.Some("").filter(evenStringLength))
  println("Some(\"abc\").filter(evenStringLength) = " + Ch04_Option.Some("abc").filter(evenStringLength))
  println("Some(\"abcd\").filter(evenStringLength) = " + Ch04_Option.Some("abcd").filter(evenStringLength))

  println("** Exercise 4.2 **")
  println("mean(emptySeq) = " + Ch04_Option.mean(emptySeq))
  println("variance(emptySeq) = " + Ch04_Option.variance(emptySeq))
  println("mean(singleSeq) = " + Ch04_Option.mean(singleSeq))
  println("variance(singleSeq) = " + Ch04_Option.variance(singleSeq))
  println("mean(fiveSeq) = " + Ch04_Option.mean(fiveSeq))
  println("variance(fiveSeq) = " + Ch04_Option.variance(fiveSeq))

  println("** Exercise 4.3 **")
  println("stringIterator(\"abc\")(0) = " + stringIterator("abc", 0))
  println("stringIterator(\"abc\")(3) = " + stringIterator("abc", 3))
  println("map2(None)(Some(23))(stringIterator) = " + Ch04_Option.map2[String, Int, String](Ch04_Option.None, Ch04_Option.Some(23))(stringIterator))
  println("map2(Some(\"a\"))(None)(stringIterator) = " + Ch04_Option.map2[String, Int, String](Ch04_Option.Some("a"), Ch04_Option.None)(stringIterator))
  println("map2(Some(\"a\"))(Some(23))(stringIterator) = " + Ch04_Option.map2[String, Int, String](Ch04_Option.Some("a"), Ch04_Option.Some(23))(stringIterator))
  println("map2(Some(\"a\"))(Some(0))(stringIterator) = " + Ch04_Option.map2[String, Int, String](Ch04_Option.Some("a"), Ch04_Option.Some(0))(stringIterator))
  println("map2(Some(\"a\"))(Some(-1))(stringIterator) = " + Ch04_Option.map2[String, Int, String](Ch04_Option.Some("a"), Ch04_Option.Some(-1))(stringIterator))

  println("** Exercise 4.4 **")
  println("sequence(Nil) = " + Ch04_Option.sequence(Nil))
  println("sequence(List(None)) = " + Ch04_Option.sequence(List(Ch04_Option.None)))
  println("sequence(List(Some(0))) = " + Ch04_Option.sequence(List(Ch04_Option.Some(0))))
  println("sequence(List(Some(0),Some(1))) = " + Ch04_Option.sequence(List(Ch04_Option.Some(0), Ch04_Option.Some(1))))
  println("sequence(List(Some(0),Some(1),Some(2),Some(3),Some(4))) = " + Ch04_Option.sequence(List(Ch04_Option.Some(0), Ch04_Option.Some(1), Ch04_Option.Some(2), Ch04_Option.Some(3), Ch04_Option.Some(4))))
  println("sequence(List(Some(0),Some(1),None,Some(2),Some(3),Some(4))) = " + Ch04_Option.sequence(List(Ch04_Option.Some(0), Ch04_Option.Some(1), Ch04_Option.None, Ch04_Option.Some(2), Ch04_Option.Some(3), Ch04_Option.Some(4))))

  println("** Exercise 4.5 **")
  println("traverse(Nil)(optionalStringLength) = " + Ch04_Option.traverse(Nil)(optionalStringLength))
  println("traverse(List(\"\",\"a\",\"b\",\"abc\",\"abcd\",\"abcde\"))(optionalStringLength) = " + Ch04_Option.traverse(List("", "a", "b", "abc", "abcd", "abcde"))(optionalStringLength))
  println("traverse(Nil)(optionalToInt) = " + Ch04_Option.traverse(Nil)(optionalToInt))
  println("traverse(List(\"0\",\"1\",\"2\",\"3\",\"4\"))(optionalToInt) = " + Ch04_Option.traverse(List("0", "1", "2", "3", "4"))(optionalToInt))
  println("traverse(List(\"0\",\"1\",\"\",\"2\",\"3\",\"4\"))(optionalToInt) = " + Ch04_Option.traverse(List("0", "1", "", "2", "3", "4"))(optionalToInt))

  println("** Exercise 4.6 **")
  // map
  println("Left(except).map(stringLength) = " + Ch04_Either.Left(except).map(stringLength))
  println("Right(\"\").map(stringLength) = " + Ch04_Either.Right("").map(stringLength))
  println("Right(\"abc\").map(stringLength) = " + Ch04_Either.Right("abc").map(stringLength))
  // flatMap
  println("Left(except).flatMap(eithernalStringLength) = " + Ch04_Either.Left(except).flatMap(eithernalStringLength))
  println("Right(\"\").flatMap(eithernalStringLength) = " + Ch04_Either.Right("").flatMap(eithernalStringLength))
  println("Right(\"abc\").flatMap(eithernalStringLength) = " + Ch04_Either.Right("abc").flatMap(eithernalStringLength))
  // orElse
  println("Left(except).orElse(Left(except)) = " + Ch04_Either.Left(except).orElse(Ch04_Either.Left(except)))
  println("Left(except).orElse(Right(1)) = " + Ch04_Either.Left(except).orElse(Ch04_Either.Right(1.0)))
  println("Left(except).orElse(Right(2.4)) = " + Ch04_Either.Left(except).orElse(Ch04_Either.Right(2.4)))
  println("Right(42).orElse(Right(2.4)) = " + Ch04_Either.Right(42).orElse(Ch04_Either.Right(2.4)))
  println("Right(Right(42)).orElse(Right(2.4)) = " + Ch04_Either.Right(Ch04_Either.Right(42)).orElse(Ch04_Either.Right(2.4)))
  // map2
  println("stringIterator(\"abc\")(0) = " + stringIterator("abc", 0))
  println("stringIterator(\"abc\")(3) = " + stringIterator("abc", 3))
  println("Left(except).map2(Right(23))(stringIterator) = " + Ch04_Either.Left(except).map2(Ch04_Either.Right(23))(stringIterator))
  println("Right(\"a\").map2(Left(except))(stringIterator) = " + Ch04_Either.Right("a").map2(Ch04_Either.Left(except))(stringIterator))
  println("Right(\"a\").map2(Right(23))(stringIterator) = " + Ch04_Either.Right("a").map2(Ch04_Either.Right(23))(stringIterator))
  println("Right(\"a\").map2(Right(0))(stringIterator) = " + Ch04_Either.Right("a").map2(Ch04_Either.Right(0))(stringIterator))
  println("Right(\"a\").map2(Right(-1))(stringIterator) = " + Ch04_Either.Right("a").map2(Ch04_Either.Right(-1))(stringIterator))

  println("** Exercise 4.7 **")
  //sequence
  println("sequence(Nil) = " + Ch04_Either.sequence(Nil))
  println("sequence(List(Left(except))) = " + Ch04_Either.sequence(List(Ch04_Either.Left(except))))
  println("sequence(List(Right(0))) = " + Ch04_Either.sequence(List(Ch04_Either.Right(0))))
  println("sequence(List(Right(0),Right(1))) = " + Ch04_Either.sequence(List(Ch04_Either.Right(0), Ch04_Either.Right(1))))
  println("sequence(List(Right(0),Right(1),Right(2),Right(3),Right(4))) = "
    + Ch04_Either.sequence(List(Ch04_Either.Right(0), Ch04_Either.Right(1), Ch04_Either.Right(2), Ch04_Either.Right(3), Ch04_Either.Right(4))))
  println("sequence(List(Right(0),Right(1),Left(except),Right(2),Right(3),Right(4))) = "
    + Ch04_Either.sequence(List(Ch04_Either.Right(0), Ch04_Either.Right(1), Ch04_Either.Left(except), Ch04_Either.Right(2), Ch04_Either.Right(3), Ch04_Either.Right(4))))
  //traverse
  println("traverse(Nil)(eithernalStringLength) = " + Ch04_Either.traverse(Nil)(eithernalStringLength))
  println("traverse(List(\"\",\"a\",\"b\",\"abc\",\"abcd\",\"abcde\"))(eithernalStringLength) = "
    + Ch04_Either.traverse(List("", "a", "b", "abc", "abcd", "abcde"))(eithernalStringLength))
  println("traverse(Nil)(eithernalToInt) = " + Ch04_Either.traverse(Nil)(eithernalToInt))
  println("traverse(List(\"0\",\"1\",\"2\",\"3\",\"4\"))(eithernalToInt) = " + Ch04_Either.traverse(List("0", "1", "2", "3", "4"))(eithernalToInt))
  println("traverse(List(\"0\",\"1\",\"\",\"2\",\"3\",\"4\"))(eithernalToInt) = "
    + Ch04_Either.traverse[String,String,Int](List("0", "1", "", "2", "3", "4"))(eithernalToInt))

}
