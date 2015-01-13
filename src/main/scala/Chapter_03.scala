import scala.annotation.tailrec
import sun.font.TrueTypeFont

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: A=>(=>B)=>B): B =  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(x)(z))(f)
  }

  def foldRight[A,B](as: List[A], z: B)(f: A=>(=>B)=>B): B =
  {
    //  println("foldRight(%s)".format(as))
    as match {
      case Nil => z
      case Cons(x, xs) => f(x)(foldRight(xs, z)(f))
    }
  }

  // I probably do not understand exercise 3.13, page 41
  def sumFoldRight(l: List[Int]) = foldRight(l, 0)(x => y => x + y)
  def productFoldRight(l: List[Int]) = foldRight(l, 1)(x => if (x == 0) _ => 0 else y => x*y)
  //

  //exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = reverse(foldLeft[A,List[B]](as,Nil)(a => bs => Cons(f(a),bs)))

  //exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = reverse(foldLeft[A,List[A]](as,Nil)(a => l => if (f(a)) Cons(a,l) else l ))
  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] = flatMap[A,A](as)(a => if (f(a)) List(a) else Nil )

  //exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = reverse(foldLeft[A,List[B]](as,Nil)(a => bs => append(f(a),bs)))

  //exercise 3.23: Pair and zipWith
  def Pair[A,B](as: List[A])(bs: List[B]):List[(A,B)] = (as,bs) match {
    case (_,Nil) => Nil
    case (Nil,_) => Nil
    case (Cons(a,ta),Cons(b,tb)) => Cons((a,b),Pair(ta)(tb))
  }
  def zipWith[A,B,C](as: List[A])(bs: List[B])(f: A => B => C): List[C] = reverse(foldLeft[Tuple2[A,B],List[C]](List.Pair(as)(bs),Nil)( a_and_b => l => Cons((f(a_and_b._1)(a_and_b._2)),l)))

  //exercise 3.24
  @tailrec
  def isInitialSegment[A](ini:List[A], as:List[A]):Boolean = ini match {
    case Nil => true
    case Cons(i,inis) => as match {
      case Nil => false
      case Cons(h,t) if i==h => isInitialSegment(inis, t)
      case _ => false
    }
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => true
    case Cons(a,as) => isInitialSegment(sub,List.dropWhile(sup)(x=>x!=a))
  }

  def concat[A](ass: List[List[A]]): List[A] = ass match {
    case Nil => Nil
    case Cons(h,t) => append(h,concat(t))
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = foldLeft(reverse(l1), l2)(a => as => Cons(a,as))

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])(a => l => Cons(a,l))

  def sumFoldLeft(l: List[Int]) = foldLeft(l, 0)(x => y => x + y)
  def productFoldLeft(l: List[Int]) = foldLeft(l, 1)(x => if (x == 0) _ => 0 else y => x*y)

  def sum(l: List[Int]): Int = l match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

  def product(ns: List[Int]): Int = ns match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x,xs) => x * product(xs)
  }

  def length[A](as: List[A]): Int = foldLeft(as, 0)(a => y => 1 + y)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l:List[A]):List[A] =  l match {
    case Nil => Nil
    case Cons(x,t) => t
  }

  @tailrec
  def drop[A](as:List[A],n:Int):List[A] =  as match {
      case Nil => Nil
      case _ if n<1 => as
      case Cons(x,t) => drop(t,n-1)
  }

  @tailrec
  def dropWhile[A](l:List[A])(f:A=>Boolean):List[A] = l match {
      case Cons(x,t) if f(x) => dropWhile(t)(f)
      case _ => l
  }

  def setHead[A](l:List[A],a:A):List[A] =  l match {
    case Nil => Nil
    case Cons(h,t) => Cons(a,t)
  }

  def init[A](l:List[A]):List[A] = l match {
      case Nil => Nil
      case Cons(x,Nil) => Nil
      case Cons(x,t) => Cons(x,init(t))
  }

  def toString[A](as:List[A]):String = {
    def go(as:List[A]):String = as match {
      case Nil => ""
      case Cons(h,Cons(a,Nil)) => h.toString+","+a
      case Cons(h,t) => h.toString+","+go(t)
    }
    "List("+go(as)+")"
  }
}

object Chapter_03{

  def matchFun(l:List[Int]):Int = l match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  def main(args: Array[String]) {

    val nilList: List[Double] = Nil
    val oneList: List[Int] = Cons(1, Nil)
    val strList: List[String] = Cons("a", Cons("b", Nil))
    val dblList: List[Double] = List(0.1,1.2,2.3,3.4,4.5)
    val fivList: List[Int] = List(0,1,2,3,4)
    val tenList: List[Int] = List(0,1,2,3,4,5,6,7,8,9)
    val hunList: List[Int] = List(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29
      ,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59
      ,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89
      ,90,91,92,93,94,95,96,97,98,99)

    println("****** Chapter_03 ******")
    println("************************")

    println("****** a few lists ******")
    println("nilList="+List.toString(nilList))
    println("oneList="+List.toString(oneList))
    println("strList="+List.toString(strList))
    println("dblList="+List.toString(dblList))
    println("fivList="+List.toString(fivList))
    println("tenList="+List.toString(tenList))
    println("hunList="+List.toString(hunList))
    println("****** playing with fivList ******")
    println("matchFun(fivList)="+matchFun(fivList))
    println("tail(fivList)="+List.tail(fivList))
    println("setHead(fivList,42)="+List.setHead(fivList,42))
    println("drop(fivList,-1)="+List.drop(fivList,-1))
    println("drop(fivList,0)="+List.drop(fivList,0))
    println("drop(fivList,3)="+List.drop(fivList,3))
    println("drop(fivList,9)="+List.drop(fivList,9))
    println("dropWhile(fivList)(i=>(i==i))="+List.dropWhile(fivList)(i=>i==i))
    println("dropWhile(fivList)(i=>(i<4))="+List.dropWhile(fivList)(i=>i<4))
    println("dropWhile(fivList)(i=>(i%3<2))="+List.dropWhile(fivList)(i=>i%3<2))
    println("****** method init ******")
    println("init(oneList)="+List.init(oneList))
    println("init(strList)="+List.init(strList))
    println("init(fivList)="+List.init(fivList))
    println("****** a few lists ******")
    println("nilList="+List.toString(nilList))
    println("oneList="+List.toString(oneList))
    println("strList="+List.toString(strList))
    println("dblList="+List.toString(dblList))
    println("fivList="+List.toString(fivList))
    println("tenList="+List.toString(tenList))
    println("hunList="+List.toString(hunList))
    println("****** foldRight and foldLeft******")
    println("sum(tenList)="+List.sum(tenList))
    println("sumFoldRight(tenList)="+List.sumFoldRight(tenList))
    println("sumFoldLeft(tenList)="+List.sumFoldLeft(tenList))
    println("product(tenList)="+List.product(tenList))
    println("productFoldRight(tenList)="+List.productFoldRight(tenList))
    println("productFoldLeft(tenList)="+List.productFoldLeft(tenList))
    println("foldRight(fivList, Nil:List[Int])(a=>as=>Cons(a,as))))="+List.foldRight(fivList, Nil:List[Int])(a=>as=>Cons(a,as)))
    println("length(fivList)="+List.length(fivList))
    println("length(tenList)="+List.length(tenList))
    println("length(hunList)="+List.length(hunList))
    println("sumFoldRight(hunList)="+List.sumFoldRight(hunList))
    println("sumFoldLeft(hunList)="+List.sumFoldLeft(hunList))
    println("product(hunList)="+List.product(hunList))
    println("productFoldRight(hunList)="+List.productFoldRight(hunList))
    println("productFoldLeft(hunList)="+List.productFoldLeft(hunList))
    println("reverse(tenList)="+List.toString(List.reverse(tenList)))
    println("append(fivList,tenList)="+List.toString(List.append(fivList,tenList)))
    println("concat(fivList,strList,tenList)="+List.toString(List.concat(List(fivList,strList,tenList))))
    println("map(fivList)(n=>n+1)="+List.toString(List.map(fivList)(n=>n+1)))
    println("map(dblList)(toString)="+List.toString(List.map(dblList)(d=>d.toString)))
    println("flatMap(fivList)(n=>List(n+1))="+List.toString(List.flatMap(fivList)(n=>List(n+1))))
    println("flatMap(fivList)(n=>List(n*n))="+List.toString(List.flatMap(fivList)(n=>List(n*n))))
    println("flatMap(fivList)(i => List(i,i)))="+List.toString(List.flatMap(fivList)(i => List(i,i))))
    println("filter[Int](Nil)(n=>0==(n%2)="+List.toString(List.filter[Int](Nil)(n=>0==(n%2))))
    println("filter(tenList)(n=>0==(n%2)="+List.toString(List.filter(tenList)(n=>0==(n%2))))
    println("filter(tenList)(n=>0==(n%7)="+List.toString(List.filter(tenList)(n=>0==(n%7))))
    println("flatFilter[Int](Nil)(n=>0==(n%2)="+List.toString(List.flatFilter[Int](Nil)(n=>0==(n%2))))
    println("flatFilter(tenList)(n=>0==(n%2)="+List.toString(List.flatFilter(tenList)(n=>0==(n%2))))
    println("flatFilter(tenList)(n=>0==(n%7)="+List.toString(List.flatFilter(tenList)(n=>0==(n%7))))
    println("Pair(fivList)(tenList)(n=>m=>n+m)="+List.toString(List.Pair(fivList)(tenList)))
    println("zipWith(fivList)(tenList)(n=>m=>n+m)="+List.toString(List.zipWith(fivList)(tenList)(n=>m=>n+m)))
    println("zipWith(fivList)(tenList)(n=>m=>n*m)="+List.toString(List.zipWith(fivList)(tenList)(n=>m=>n*m)))
    println("fivList="+fivList)
    println("tenList="+tenList)
    println("isInitialSegment(fivList,tenList,true)="+List.isInitialSegment(fivList,tenList))
    println("isInitialSegment(tenList,fivList,true)="+List.isInitialSegment(tenList,fivList))
    println("hasSubsequence(fivList,tenList)="+List.hasSubsequence(fivList,tenList))
    println("hasSubsequence(tenList,fivList)="+List.hasSubsequence(tenList,fivList))
    println("hasSubsequence(hunList,fivList)="+List.hasSubsequence(hunList,fivList))
    println("hasSubsequence(hunList,tenList)="+List.hasSubsequence(hunList,tenList))
    println("hasSubsequence(List(0,42,1,2,3,4),fivList)="+List.hasSubsequence(List(0,42,1,2,3,4),fivList))
  }
}
