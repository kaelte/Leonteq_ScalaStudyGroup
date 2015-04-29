import java.util.Calendar
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

object Ch07 {

  object Phase1 {

    trait Par[A]

    def unit[A](a: => A): Par[A] = ???

    def get[A](a: Par[A]): A = ???

    // 7.1 Par.map2 is a new higher-order function for combining the result of two parallel computations.
    // What is its signature? Give the most general signature possible (don’t assume it works only for Int).
  }


  object Phase2 {

    trait Par[A]

    def unit[A](a: A): Par[A] = ???

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

    def fork[A](a: => Par[A]): Par[A] = ???

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](a: Par[A]): A = ???

    // 7.2 Before continuing, try to come up with representations for Par that make it possible
    // to implement the functions of our API.
  }

  object Phase3 {

    type Par[A] = ExecutorService => Future[A]

    /*

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}


trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}
 */

    object Par {
      def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

      private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true

        def get(timeout: Long, units: TimeUnit) = get

        def isCancelled = false

        def cancel(evenIfRunning: Boolean): Boolean = false
      }

      def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

      def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        (es: ExecutorService) => {
          val af = a(es)
          val bf = b(es)
          UnitFuture(f(af.get, bf.get))
        }


      def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
          def call = a(es).get
        })

      // 7.3 Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.

      /**
       * This is the implementation from the official solution.
       * I did not understand the exercise, actually the whole template
       * It does not suffice to fix map2, the official solution introduces a new class and so on
       * Moreover some stuff from the standard Java library needs to be imported.
       * How shall one know this ? I thought the book does not require any prior experience with
       * Scala as it is written on page xviii but that seems to be utterly wrong.
       */
      case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                     f: (A, B) => C) extends Future[C] {
        @volatile var cache: Option[C] = None

        def isDone = cache.isDefined

        def isCancelled = a.isCancelled || b.isCancelled

        def cancel(evenIfRunning: Boolean) =
          a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

        def get = compute(Long.MaxValue)

        def get(timeout: Long, units: TimeUnit): C =
          compute(TimeUnit.MILLISECONDS.convert(timeout, units))

        private def compute(timeoutMs: Long): C = cache match {
          case Some(c) => c
          case None =>
            //            println("...compute: timeoutMs=" + timeoutMs)
            val start = System.currentTimeMillis
            val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
            val stop = System.currentTimeMillis;
            val at = stop - start
            val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
            val ret = f(ar, br)
            cache = Some(ret)
            ret
        }
      }

      def map2Timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        (es: ExecutorService) => {
          val af: Future[A] = a(es)
          val bf: Future[B] = b(es)
          Map2Future(af, bf, f)
        }


      // 7.4 This API already enables a rich set of operations. Here’s a simple example: using lazyUnit,
      // write a function to convert any function A => B to one that evaluates its result asynchronously.

      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

      def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

      // 7.5 Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
      def nilPar[A]: Par[List[A]] = unit(List.Nil)
      def consPar[A](aPar :Par[A])(asPar: => Par[List[A]]) : Par[List[A]] =  exi => UnitFuture(List.Cons(aPar(exi).get,asPar(exi).get))
      def sequence[A](ps: List[Par[A]]): Par[List[A]] = List.foldLeft[Par[A],Par[List[A]]](List.reverse(ps),nilPar)(consPar)

    }

  }

}


object nith_Chapter_07 extends App {

  val log: Any => Unit = x => println(Calendar.getInstance().getTime() + "  " + x.toString)
  val logException: Exception => Any => Unit = e => x => println(Calendar.getInstance().getTime() + "  " + x + " = " + e)
  lazy val intSign: (Boolean, Int) => Int = (p, n) => if (p) n else 0 - n

  // It would be nice if the red book did say how to create an ExecutorService.
  // I found this line in the comment of Executors.java but can speculate only
  // what it does.

  lazy val esDaemon: ExecutorService = Executors.newFixedThreadPool(4, new ThreadFactory {
    val counter = new AtomicInteger(0)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(r, s"PAR-thread-${counter.getAndIncrement}")
      t.setDaemon(true)
      t
    }
  })
  lazy val es: ExecutorService = Executors.newFixedThreadPool(4)

  lazy val oneStream: Ch05.Stream[Int] = Ch05.Stream.cons(1, oneStream)
  val isThereTwo: Ch05.Stream[Int] => Boolean = _.exists(_ == 2)
  lazy val nonTerminatingBool = isThereTwo(oneStream)
  lazy val twoPar: Ch07.Phase3.Par[Int] = Ch07.Phase3.Par.unit(2)
  lazy val threePar: Ch07.Phase3.Par[Int] = Ch07.Phase3.Par.unit(3)
  lazy val nonTerminatingCall: Callable[Boolean] = new Callable[Boolean] {
    def call = nonTerminatingBool
  }
  lazy val infinitePar: Ch07.Phase3.Par[Boolean] = execService => execService.submit[Boolean](nonTerminatingCall)

  println("****** Chapter_07 ******")
  println("Long.MaxValue    = %s".format(Long.MaxValue))
  println("twoPar(es).get   = " + twoPar(es).get)
  println("threePar(es).get = " + threePar(es).get)
  println("intSign: (Boolean, Int) => Int   =   (p, n) => if (p) n else 0 - n")

  println("\n** Exercise 7.3 **")
  log("map2Timeout(twoPar,threePar)(_ * _)(es).get = " + Ch07.Phase3.Par.map2Timeout(twoPar,threePar)(_ * _)(es).get)
  log("map2Timeout(twoPar,threePar)(_ * _)(es).get(1,TimeUnit.SECONDS) = " + Ch07.Phase3.Par.map2Timeout(twoPar,threePar)(_ * _)(es).get(1, TimeUnit.SECONDS))
  try {
    log(Ch07.Phase3.Par.map2Timeout(infinitePar,threePar)(intSign)(esDaemon).get(2,TimeUnit.SECONDS))
  } catch {
    case e: Exception => logException(e)("map2Timeout(infinitePar,threePar)(intSign)(esDaemon).get(2,TimeUnit.SECONDS)")
      es.shutdownNow()
  }

  println("\n** Exercise 7.4 **")
  try {
    log(Ch07.Phase3.Par.asyncF(isThereTwo)(oneStream)(esDaemon).get(2,TimeUnit.SECONDS))
  } catch {
    case e: Exception => logException(e)("asyncF(isThereTwo)(oneStream)(esDaemon).get(2,TimeUnit.SECONDS)")
      es.shutdownNow()
  }

  println("\n** Exercise 7.5 **")
  log("sequence(List.Nil)(es).get(2,TimeUnit.SECONDS) = " + Ch07.Phase3.Par.sequence(List.Nil)(es).get(2,TimeUnit.SECONDS))
  log("sequence(List(twoPar,threePar))(es).get(2,TimeUnit.SECONDS) = " + List.myString(Ch07.Phase3.Par.sequence(List(twoPar,threePar))(es).get(2,TimeUnit.SECONDS)))

  log("*** Not finished yet ***")

}
