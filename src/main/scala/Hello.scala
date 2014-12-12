object Hello {

  def max(x: Int, y: Int): Int = {if (x>y) x else y}
  def maxCur(x: Int) : (Int => Int) = {y => if (x>y) x else y}

  val maxVal = (x: Int, y: Int) => if (x>y) x else y
  val maxCurVal = (x : Int) => (y: Int) => if (x > y) x else y

  def main(args: Array[String]) {
    println("Hello World! " + " Here are the "+ args.size +" arguments: ")
    args.foreach(arg => println(arg)) ;

    println("And the maximum of {0,1} calculated in different ways")
    println("max(0,1)=" + max(0,1))
    println("maxCur(0,1)=" + maxCur(0)(1))
    println("maxVal(0,1)=" + maxVal(0,1))
    println("maxCurVal(3,2)=" + maxCurVal(3)(2))
  }
}
