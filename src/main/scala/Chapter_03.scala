import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
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

  def tail[A](l:List[A]):List[A] =  l match {
    case Nil => Nil
    case Cons(x,t) => t
  }

  def drop[A](as:List[A],n:Int):List[A] =  {
    @tailrec
    def go(i: Int,l:List[A]):List[A] = l match {
      case Nil => Nil
      case Cons(x,t) => if (i<1) l else go(i-1,t)
    }
    go(n,as)
  }

  def dropWhile[A](l:List[A],f:A=>Boolean):List[A] =  {
    @tailrec
    def go(as:List[A]):List[A] =  as match {
      case Nil => Nil
      case Cons(x,t) => if (f(x)) go(tail(as)) else as
    }
    go(l)
  }

  def setHead[A](l:List[A],a:A):List[A] =  l match {
    case Nil => Nil
    case Cons(x,t) => Cons(a,t)
  }

  def init[A](l:List[A]):List[A] =  {
//    @tailrec --- :(
    def go(as:List[A]):List[A] =  as match {
      case Nil => Nil
      case Cons(x,Nil) => Nil
      case Cons(x,Cons(y,Nil)) => Cons(x,Nil)
      case Cons(x,t) => Cons(x,go(t))
    }
    go(l)
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
    val fivList: List[Int] = List(1,2,3,4,5)

    println("****** Chapter_03 ******")
    println("************************")

    println("****** a few lists ******")
    println("nilList="+nilList)
    println("oneList="+oneList)
    println("strList="+strList)
    println("fivList="+fivList)
    println("****** playing with fivList ******")
    println("matchFun(fivList)="+matchFun(fivList))
    println("tail(fivList)="+List.tail(fivList))
    println("setHead(fivList,42)="+List.setHead(fivList,42))
    println("drop(fivList,-1)="+List.drop(fivList,-1))
    println("drop(fivList,0)="+List.drop(fivList,0))
    println("drop(fivList,3)="+List.drop(fivList,3))
    println("drop(fivList,9)="+List.drop(fivList,9))
    println("dropWhile(fivList,(fivList,(i:Int)=>(i<4)))="+List.dropWhile(fivList,(i:Int)=>(i<4)))
    println("dropWhile(fivList,(fivList,(i:Int)=>(i<==i)))="+List.dropWhile(fivList,(i:Int)=>(i==i)))
    println("init(fivList)="+List.init(fivList))
  }
}