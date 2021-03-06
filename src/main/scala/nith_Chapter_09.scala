package fp_nith
import List.Cons
import util._

import scala.util.matching.Regex
//import scala.annotation.tailrec

object Ch9 {

  trait Parsers[ParseError, Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError,A]

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    /// primitive combinators
    def string(s: String): Parser[String]
    implicit def regex(r: Regex): Parser[String]
    def slice[A](p: Parser[A]): Parser[String]
    def succeed[A](a: A): Parser[A] // could be implemented using map but we want it to be primitive = string("") map (_ => a)
    def or[A,B >: A](p1: Parser[A])(p2: => Parser[B]): Parser[B]
    def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]




    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))




    // 9.1 Using product, implement the now-familiar combinator map2 and then use this to implement many1 in terms of
    // many. Note that we could have chosen to make map2 primitive and defined product in terms of map2 as we’ve done
    // in previous chapters. The choice is up to you.
    def map2[A,B,C](p1: Parser[A])(p2: => Parser[B])(f: A => B => C): Parser[C] =
      product[A,B](p1)(p2).map[C]( x => f(x._1)(x._2))
    def many1[A](p: Parser[A]): Parser[List[A]] = p.product[List[A]](many[A](p)).map[List[A]](x => Cons(x._1,x._2))



    // 9.3 Hard: Before continuing, see if you can define many in terms of or, map2, and succeed.
    def many[A](p: Parser[A]): Parser[List[A]] =  {
      val nilSuc:Parser[List[A]] = succeed[List[A]](List.Nil)
      nilSuc.or(p.map2[List[A],List[A]](p2 = many[A](p))(a => as => List.Cons[A](a,as)))
    }

    // 9.4
    def listOfN[A](n: Int,p: Parser[A]): Parser[List[A]] = {
      def go[A](n: Int, p: Parser[A]): Parser[List[A]] = {
        if (0 == n) succeed[List[A]](List())
        else p.map2[List[A],List[A]](go[A](n-1,p))(a => as => Cons[A](a,as))
      }
      go(n,p)
    }

    // 9.6
    def thatMany[A](p: Parser[A]) = regex("[0-9]".r).map[Int](_.toInt).flatMap[List[A]](listOfN[A](_,p))

    // 9.7
    def product[A,B](p1: Parser[A])(p2: => Parser[B]): Parser[(A,B)] = p1.flatMap[(A,B)](a => p2.map[(A,B)]((a,_)))
    def map22[A,B,C](p1: Parser[A])(p2: => Parser[B])(f: A => B => C): Parser[C] =
      p1.flatMap[C](a => p2.map[(A,B)]((a,_)).map[C](ab => f(ab._1)(ab._2)))

    // 9.8
    def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap[B](a => succeed[B](f(a)))



    case class ParserOps[A](p: Parser[A]) {
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or[A,B](p)(p2)
      def |[B >: A](p2: Parser[B]): Parser[B] = or(p2)

      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

      def many[B >: A]: Parser[List[B]] = self.many(p)
      def many1[B >: A]: Parser[List[B]] = self.many1(p)

      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def map2[B,C](p2: Parser[B])(f: A => B => C): Parser[C] = self.map2[A,B,C](p)(p2)(f)

      def product[B](p2: Parser[B]): Parser[(A,B)] = self.product[A,B](p)(p2)
      def **[B](p2: Parser[B]): Parser[(A,B)] = product[B](p2)
    }

    val numA: Parser[Int] = char('a').many.map(l => List.length(l))
    val numA1: Parser[Int] = char('a').many1.map(l => List.length(l))

    object Laws {
      import Ch08.Phase2.Gen
      import Ch08.Phase3.{forAll,forAllAll,Prop}

      final def equal[A](p1: Parser[A])(p2: Parser[A])(in: Int => Gen[String]): Prop =
        forAll[String](in)(s => run(p1)(s) == run(p2)(s))

      final def mapLaw[A](p: Parser[A])(in: Int => Gen[String]): Prop = equal(p)(p.map(a => a))(in)

      //9.2 Hard: Try coming up with laws to specify the behavior of product.
      final def prodAssociative[A](p1: Parser[A])(p2: Parser[A])(p3: Parser[A])(in: Int => Gen[String]): Prop = {
        val rightLeft: Parser[(A, (A, A))] = p1.**(p2.**(p3))
        val leftRight: Parser[(A, (A, A))] = p1.**(p2).**(p3).map(x => (x._1._1, (x._1._2, x._2)))
        equal[(A, (A, A))](rightLeft)(leftRight)(in)
      }

      final def prodNeutralElementRight[A,B](p: Parser[A])(in1: Int => Gen[B])(in2: Int => Gen[String]): Prop = {
        val pTimesSucceed: B=>Parser[A] = b=>p.**(succeed[B](b)).map[A](x => x._1)
        forAllAll[B](in1)(b => equal[A](p)(pTimesSucceed(b))(in2))
      }

      final def prodNeutralElementLeft[A,B](p: Parser[A])(in1: Int => Gen[B])(in2: Int => Gen[String]): Prop = {
        val succeedTimesP: B=>Parser[A] = b=>succeed[B](b).**(p).map[A](x => x._2)
        forAllAll[B](in1)(b => equal[A](p)(succeedTimesP(b))(in2))
      }

      final def prodCommutative[A](p1: Parser[A])(p2: Parser[A])(in: Int => Gen[String]): Prop =
        equal[(A,A)](p1.**(p2))(p1.**(p2))(in)

    }

  }


}


object nith_Chapter_09 extends App {

  println("****** Chapter_09 ******")

  println("\n** Exercise 9 XYZ **")
  logg("XXX")("Coming soon !")

  println()
  println("*** Not finished yet ***")
}
