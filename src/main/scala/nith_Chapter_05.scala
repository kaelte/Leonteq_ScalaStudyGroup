import scala.annotation.tailrec

object Ch05 {


  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def myString: String = {
      def go[A](sa: Stream[A]):String = sa match {
        case Empty => ""
        case Cons(h, t) => if (t() == Empty) h().toString else h().toString + "," + go[A](t())
      }
      "("+go(this)+")"
    }

    // QUESTION: Why do we want to force the evaluation ? Laziness is cool, isn't it ?
    // 5.1 Implement toList function that convert a Stream to a List, which will force its evaluation and let you
    // look at it in the REPL. You can convert to the regular List type in the standard library.
    def toList: List[A] = {
      @tailrec
      def go(str: Stream[A])(as: List[A]): List[A] = str match {
        case Empty => as
        case Cons(h, t) => go(t())(List.Cons[A](h(), as))
      }
      List.reverse(go(this)(List.Nil))
    }

    // 5.2 Write the function take(n) for returning the first n elements of a Stream,
    // and drop(n) for skipping the first n elements of a Stream.
    final def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (n>0) Cons(h, ()=>t().take(n-1)) else Empty
    }
    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (n>0) t().drop(n-1) else Cons(h,t)
    }

    // 5.3 Write the function takeWhile for returning all starting elements of
    // a Stream that match the given predicate.
    final def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (p(h())) Cons(h, ()=>t().takeWhile(p)) else t().takeWhile(p)
    }

    // 5.4 Implement forAll, which checks that all elements in the Stream match a given predicate.
    // Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    final def forAll(p: A => Boolean): Boolean = !this.exists(a => !p(a))
    // One could implement exists using foldRight. However foldRight is not tail recursive :(
    @tailrec
    final def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    // 5.5 Use foldRight to implement takeWhile.
    final def takeWhileFoldRight(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (p(h())) Cons(h, ()=>t().takeWhile(p)) else t().takeWhileFoldRight(p)
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

    def tails: Stream[Stream[A]] = ???

    def startsWith[B](s: Stream[B]): Boolean = ???

    def hasSubsequence[B](s: Stream[B]): Boolean =
      tails exists (_ startsWith s)

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
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def apply[A](f:Int => A): Stream[A] = {
    def go[A](f:Int => A)(n:Int): Stream[A] = Cons(()=>f(n),()=>go(f)(n+1))
    go(f)(0)
  }
  }


  // 5.6 Hard: Implement headOption using foldRight.


  // 5.7 Implement map, filter, append, and flatMap using foldRight.
  // The append method should be non-strict in its argument.


  // 5.8 Generalize ones slightly to the function constant, which
  // returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = ???


  // 5.9 Write a function that generates an infinite stream of integers,
  // starting from n, then n + 1, n + 2, and so on.7
  def from(n: Int): Stream[Int] = ???


  // 5.10 Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.


  // 5.11 Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing both the next state
  // and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???


  // 5.12 Write fibs, from, constant, and ones in terms of unfold.


  // 5.13 Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3),
  // and zipAll. The zipAll function should continue the traversal as long as either
  // stream has more elements—it uses Option to indicate whether each stream has been exhausted.


  // 5.14 Hard: Implement startsWith using functions you’ve written. It should check
  // if one Stream is a prefix of another. For instance, Stream(1,2,3)
  // startsWith Stream(1,2) would be true.


  // 5.15 Implement tails using unfold. For a given Stream, tails returns the Stream of
  // suffixes of the input sequence, starting with the original Stream. For example,
  // given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).


  // 5.16 Hard: Generalize tails to the function scanRight, which is like a foldRight that returns
  // a stream of the intermediate results. For example:
  // scala> Stream(1,2,3).scanRight(0)(_ + _).toList
  // res0: List[Int] = List(6,5,3,0)
  // This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0).
  // Your function should reuse intermediate results so that traversing a Stream with n elements
  // always takes time linear in n. Can it be implemented using unfold? How, or why not? Could it
  // be implemented using another function we’ve written?

}

object nith_Chapter_05 extends App {

  val oneStream: Ch05.Stream[Int] = Ch05.Cons[Int](()=>0,()=>Ch05.Empty)
  val fivStream: Ch05.Stream[Int] = Ch05.Stream(0, 1, 2, 3, 4)
  val identityStream: Ch05.Stream[Int] = Ch05.Stream[Int]((n:Int)=>n)
  val squareStream: Ch05.Stream[Int] = Ch05.Stream[Int]((n:Int)=>n*n)

  println("****** Chapter_05 ******")

  println("Empty = " + Ch05.Empty.myString)
  println("fivStream = " + fivStream.myString)

  println("** Exercise 5.1 **")
  println("Empty.toList = " + Ch05.Empty.toList)
  println("fivStream.toList = " + fivStream.toList)
  println("** Exercise 5.2 **")
  println("** take ")
  println("Empty.take(1) = " + Ch05.Empty.take(1).myString)
  println("fivStream.take(-1) = " + fivStream.take(-1).myString)
  println("fivStream.take(0) = " + fivStream.take(0).myString)
  println("fivStream.take(3) = " + fivStream.take(3).myString)
  println("fivStream.take(7) = " + fivStream.take(7).myString)
  println("identityStream.take(42) = " + identityStream.take(42).myString)
  println("squareStream.take(42) = " + squareStream.take(42).myString)
  println("** drop ")
  println("Empty.drop(1) = " + Ch05.Empty.drop(1).myString)
  println("fivStream.drop(-1) = " + fivStream.drop(-1).myString)
  println("fivStream.drop(0) = " + fivStream.drop(0).myString)
  println("fivStream.drop(3) = " + fivStream.drop(3).myString)
  println("fivStream.drop(7) = " + fivStream.drop(7).myString)
  println("identityStream.take(42).drop(10) = " + identityStream.take(42).drop(10).myString)
  println("squareStream.take(42).drop(10) = " + squareStream.take(42).drop(10).myString)
  println("** Exercise 5.3 **")
  println("Empty.takeWhile(1) = " + Ch05.Empty.takeWhile(_=>true).myString)
  println("fivStream.takeWhile(_%2==0) = " + fivStream.takeWhile(_%2==0).myString)
  println("identityStream.take(42).takeWhile(_%2==0) = " + identityStream.take(42).takeWhile(_%2==0).myString)
  println("squareStream.take(42).takeWhile(_%2==0) = " + squareStream.take(42).takeWhile(_%2==0).myString)
  println("** Exercise 5.4 **")
  println("fivStream.forAll(_<6) = " + fivStream.forAll(_<6))
  println("fivStream.forAll(_%2==0) = " + fivStream.forAll(_%2==0))
  println("identityStream.forAll(_<1000) = " + identityStream.forAll(_<1000))
  println("squareStream.forAll(_<10000) = " + squareStream.forAll(_<10000))
  println("** Exercise 5.5 **")
  println("!!!!! NOT FINISHED !!!!!")
  println("Empty.takeWhileFoldRight(1) = " + Ch05.Empty.takeWhileFoldRight(_=>true).myString)
  println("fivStream.takeWhileFoldRight(_%2==0) = " + fivStream.takeWhileFoldRight(_%2==0).myString)
  println("identityStream.take(42).takeWhileFoldRight(_%2==0) = " + identityStream.take(42).takeWhileFoldRight(_%2==0).myString)
  println("squareStream.take(42).takeWhileFoldRight(_%2==0) = " + squareStream.take(42).takeWhileFoldRight(_%2==0).myString)
  println("!!!!! NOT FINISHED !!!!!")
}
