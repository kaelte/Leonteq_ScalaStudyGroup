
object Chapter04 {

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
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(a => mean(xs.map(x => math.pow(x-a,2))))


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

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y=>f(x,y)))



  // 4.4 Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list. If the original list contains None even
  // once, the result of the function should be None; otherwise the result should be Some with a
  // list of all the values. Here is its signature:
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Cons(None, t) => None
    case Cons(Some(h), t) => map2[A,List[A],List[A]](Some(h),sequence(t))((h,t)=>Cons(h,t))
  }



  /*
    def parseInts(a: List[String]): Option[List[Int]] =
      sequence(a map (i => Try(i.toInt)))
  */


  // 4.5 Implement traverse. It’s straightforward to do using map and sequence, but try for a more efficient
  // implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =  a match {
    case Nil => Some(Nil)
    case Cons(h, t) => f(h) match {
      case None => None
      case Some(b) => map2[B,List[B],List[B]](Some(b),traverse(t)(f))((h,t)=>Cons(h,t))
    }
  }

}

object Chapter04_Either {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = ???

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
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


  // 4.6 Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.


  // 4.7 Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???


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
  // QUESTION: I could also use Some as value. What is better ?
  // val optionalStringLength: String => Option[Int] = s => Some(s.length)
  val optionalStringLength: String => Chapter04.Option[Int] = s => Chapter04.Some(s.length)
  val optionalToInt: String => Chapter04.Option[Int] = x => Chapter04.Try{x.toInt}
  val evenStringLength: String => Boolean = s => s.length%2 == 0
  val stringIterator: (String,Int) => String = (s,i) => if (i<1) "" else s + stringIterator(s,i-1)
// sequences
  val emptySeq: Seq[Double] = Seq()
  val singleSeq: Seq[Double] = Seq(42)
  val fiveSeq: Seq[Double] = Seq(0,1,2,3,4)

  println("************************")
  println("****** Chapter_04 ******")
  println("************************")

  println("** Exercise 4.1 **")
  // map
  println("None.map(stringLength) = " + Chapter04.None.map(stringLength))
  println("Some(\"\").map(stringLength) = " + Chapter04.Some("").map(stringLength))
  println("Some(\"abc\").map(stringLength) = " + Chapter04.Some("abc").map(stringLength))
  // getOrElse
  println("None.getOrElse(\"\") = " + Chapter04.None.getOrElse(""))
  println("None.getOrElse(None) = " + Chapter04.None.getOrElse(None))
  println("Some(\"\").getOrElse(\"\") = " + Chapter04.Some("").getOrElse(""))
  println("Some(\"abc\").getOrElse(\"\") = " + Chapter04.Some("abc").getOrElse(""))
  println("Some(Some(\"abc\")).getOrElse(\"\") = " + Chapter04.Some(Chapter04.Some("abc")).getOrElse(""))
  // map
  println("None.flatMap(optionalStringLength) = " + Chapter04.None.flatMap[Int](optionalStringLength))
  println("Some(\"\").flatMap(optionalStringLength) = " + Chapter04.Some("").flatMap(optionalStringLength))
  println("Some(\"abc\").flatMap(optionalStringLength) = " + Chapter04.Some("abc").flatMap(optionalStringLength))
  // orElse
  println("None.orElse(None) = " + Chapter04.None.orElse(Chapter04.None))
  println("None.orElse(Some(1)) = " + Chapter04.None.orElse(Chapter04.Some(1.0)))
  println("None.orElse(Some(2.4)) = " + Chapter04.None.orElse(Chapter04.Some(2.4)))
  println("Some(42).orElse(Some(2.4)) = " + Chapter04.Some(42).orElse(Chapter04.Some(2.4)))
  println("Some(Some(42)).orElse(Some(2.4)) = " + Chapter04.Some(Chapter04.Some(42)).orElse(Chapter04.Some(2.4)))
  // filter
  println("None.filter(evenStringLength) = " + Chapter04.None.filter(evenStringLength))
  println("Some(\"\").filter(evenStringLength) = " + Chapter04.Some("").filter(evenStringLength))
  println("Some(\"abc\").filter(evenStringLength) = " + Chapter04.Some("abc").filter(evenStringLength))
  println("Some(\"abcd\").filter(evenStringLength) = " + Chapter04.Some("abcd").filter(evenStringLength))

  println("** Exercise 4.2 **")
  println("mean(emptySeq) = " + Chapter04.mean(emptySeq))
  println("variance(emptySeq) = " + Chapter04.variance(emptySeq))
  println("mean(singleSeq) = " + Chapter04.mean(singleSeq))
  println("variance(singleSeq) = " + Chapter04.variance(singleSeq))
  println("mean(fiveSeq) = " + Chapter04.mean(fiveSeq))
  println("variance(fiveSeq) = " + Chapter04.variance(fiveSeq))

  println("** Exercise 4.3 **")
  println("stringIterator(\"abc\")(0) = " + stringIterator("abc",0))
  println("stringIterator(\"abc\")(3) = " + stringIterator("abc",3))
  println("map2(None)(Some(23))(stringIterator) = " + Chapter04.map2[String,Int,String](Chapter04.None,Chapter04.Some(23))(stringIterator))
  println("map2(Some(\"a\"))(None)(stringIterator) = " + Chapter04.map2[String,Int,String](Chapter04.Some("a"),Chapter04.None)(stringIterator))
  println("map2(Some(\"a\"))(Some(23))(stringIterator) = " + Chapter04.map2[String,Int,String](Chapter04.Some("a"),Chapter04.Some(23))(stringIterator))
  println("map2(Some(\"a\"))(Some(0))(stringIterator) = " + Chapter04.map2[String,Int,String](Chapter04.Some("a"),Chapter04.Some(0))(stringIterator))

  println("** Exercise 4.4 **")
  println("sequence(Nil) = " + Chapter04.sequence(Nil))
  println("sequence(List(None)) = " + Chapter04.sequence(List(Chapter04.None)))
  println("sequence(List(Some(0))) = " + Chapter04.sequence(List(Chapter04.Some(0))))
  println("sequence(List(Some(0),Some(1))) = " + Chapter04.sequence(List(Chapter04.Some(0),Chapter04.Some(1))))
  println("sequence(List(Some(0),Some(1),Some(2),Some(3),Some(4))) = " + Chapter04.sequence(List(Chapter04.Some(0),Chapter04.Some(1),Chapter04.Some(2),Chapter04.Some(3),Chapter04.Some(4))))
  println("sequence(List(Some(0),Some(1),None,Some(2),Some(3),Some(4))) = " + Chapter04.sequence(List(Chapter04.Some(0),Chapter04.Some(1),Chapter04.None,Chapter04.Some(2),Chapter04.Some(3),Chapter04.Some(4))))

  println("** Exercise 4.5 **")
  println("traverse(Nil)(optionalStringLength) = " + Chapter04.traverse(Nil)(optionalStringLength))
  println("traverse(List(\"\",\"a\",\"b\",\"abc\",\"abcd\",\"abcde\"))(optionalStringLength) = " + Chapter04.traverse(List("","a","b","abc","abcd","abcde"))(optionalStringLength))
  println("traverse(Nil)(optionalToInt) = " + Chapter04.traverse(Nil)(optionalToInt))
  println("traverse(List(\"0\",\"1\",\"2\",\"3\",\"4\"))(optionalToInt) = " + Chapter04.traverse(List("0","1","2","3","4"))(optionalToInt))
  println("traverse(List(\"0\",\"1\",\"\",\"2\",\"3\",\"4\"))(optionalToInt) = " + Chapter04.traverse(List("0","1","","2","3","4"))(optionalToInt))

}
