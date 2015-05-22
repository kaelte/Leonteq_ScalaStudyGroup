import util.log
import Ch04_Option.{Option, Some}
import Ch05.{Stream, unfold, unfold2}
import Ch06.{double, doubleRandState, nonNegativeLessThanState, RandState, RNG, sequence, SimpleRNG, State, unitState}

object Ch08 {

  // 8.1 To get used to thinking about testing in this way, come up with properties that specify the implementation
  // of a sum: List[Int] => Int function. You don’t have to write your properties down as executable ScalaCheck
  // code an informal description is fine.

  // 8.2 What properties specify a function that finds the maximum of a List[Int]?

  object Phase1 {

    // 8.3 Assuming the following representation of Prop, implement && as a method of Prop.
    trait Prop {
      def check: Boolean

      def &&(p: Prop): Prop = new Prop {
        def check: Boolean = this.check && p.check
      }
    }

  }

  object Phase2 {
    type FailedCase = String
    type SuccessCount = Int

    /*    trait Prop {
          def check: Either[(FailedCase, SuccessCount), SuccessCount]
        }*/

    case class Gen[+A](sample: State[RNG, A]) {
      // 8.6 Implement flatMap, and then use it to implement this more dynamic version of listOfN.
      def flatMap[B](f: A => Gen[B]): Gen[B] = new Gen[B](this.sample.flatMap[B](f(_).sample))

      def listOfN(size: Gen[Int]): Gen[List[A]] = {
        val runOfAs: RNG => (List[A], RNG) = rng => sequence[RNG, A](List.fill[RandState[A]](size.sample.run(rng)._1)(this.sample)).run(rng)
        val stateOfAs: State[RNG, List[A]] = new State[RNG, List[A]](runOfAs)
        new Gen[List[A]](stateOfAs)
      }

      // 8.10 Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
      def unsized: SGen[A] = new SGen[A](n => this)
    }

    // 8.4 Implement Gen.choose using this representation of Gen. It should generate integers in the range
    // start to stopExclusive. Feel free to use functions you’ve already written.
    def choose(start: Int, stopExclusive: Int): Gen[Int] = new Gen[Int](nonNegativeLessThanState(stopExclusive - start).map[Int](_ + start))

    final def choose(start: Double, stopExclusive: Double): Gen[Double] = new Gen[Double](doubleRandState.map[Double](x => (stopExclusive - start) * x + start))

    // 8.5 Let’s see what else we can implement using this representation of Gen.
    // Try implementing unit, boolean, and listOfN.
    final def unit[A](a: => A): Gen[A] = new Gen[A](unitState[RNG, A](a))

    final def boolean: Gen[Boolean] = new Gen[Boolean](new State[RNG, Boolean](rng => rng.nextInt match {
      case (n, r) => (n % 2 == 0, r)
    }))

    final def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val stateOfAs: State[RNG, List[A]] = sequence[RNG, A](List.fill[RandState[A]](n)(g.sample))
      new Gen[List[A]](stateOfAs)
    }


    // 8.7 Implement union, for combining two generators of the same type into one,
    // by pulling values from each generator with equal likelihood.
    final def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap[A](p => if (p) g1 else g2)

    final def unionLazy[A](g1: => Gen[A], g2: => Gen[A]): Gen[A] = boolean.flatMap[A](p => if (p) g1 else g2)

    // 8.8 Implement weighted, a version of union that accepts a weight for each Gen and generates values
    // from each Gen with probability proportional to its weight.
    final def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = choose(0.0 - g1._2, g2._2).flatMap[A](x => if (x < 0) g1._1 else g2._1)

    final def weighted2[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val g1weight: Double = g1._2 / (g1._2 + g2._2)
      val doubleGen: Gen[Double] = new Gen[Double](doubleRandState)
      log("...weighted : g1weight=" + g1weight)
      doubleGen.flatMap[A]((x => if (x < g1weight) g1._1 else g2._1))
    }

    type TestCases = Int

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      final def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      final def isFalsified = true
    }

    case class Prop(run: (TestCases, RNG) => Result) {
      // EXERCISE 8.9
      // Now that we have a representation of Prop, implement && and || for composing Prop values. Notice that in the
      // case of failure we don’t know which property was responsible, the left or the right. Can you devise a way of
      // handling this, perhaps by allowing Prop values to be assigned a tag or label which gets displayed in the event
      // of a failure?
      final def &&(p: Prop): Prop = {
        def newRunner(tc: TestCases, r: RNG): Result = this.run(tc, r) match {
          case Passed => p.run(tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: LEFT prop passed but RIGHT Prop falsified: " + rFail, rSuc)
            case Passed => Passed
          }
          case Falsified(lFail, lSuc) => p.run(tc, r) match {
            case Passed => new Falsified("Prop.&&: RIGHT Prop passed but LEFT Prop falsified: " + lFail, lSuc)
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props falsified: (LEFT prop after " + lSuc + " test cases: " + lFail + ", RIGHT prop after " + rSuc + " test cases: " + rFail + ")", lSuc.min(rSuc))
          }
        }
        Prop(newRunner)
      }

      final def ||(p: Prop): Prop = {
        def newRunner(tc: TestCases, r: RNG): Result = this.run(tc, r) match {
          case Passed => Passed
          case Falsified(lFail, lSuc) => p.run(tc, r) match {
            case Passed => Passed
            case Falsified(rFail, rSuc) => new Falsified("Prop.||: BOTH Props falsified: (LEFT prop after " + lSuc + " test cases: " + lFail + ", RIGHT prop after " + rSuc + " test cases: " + rFail + ")", lSuc.max(rSuc))
          }
        }
        Prop(newRunner)
      }
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, rng) => {
        val lazyIdentiy: (=> Int) => Int = n => n
        lazy val indexedAstream: Stream[(A, Int)] = randomStream[A](as)(rng).zip[Int](Stream(lazyIdentiy).take(n))
        lazy val mapli: (=> Tuple2[A, Int]) => Result = x => try {
          if (f(x._1)) Passed else Falsified(x._1.toString, x._2)
        } catch {
          case e: Exception => Falsified(buildMsg(x._1, e), x._2)
        }
        lazy val resultStream: Stream[Result] = indexedAstream.map[Result](mapli)
        resultStream.find(_.isFalsified).getOrElse(Passed)
      }
    }

    case class SGen[+A](forSize: Int => Gen[A]) {
      // 8.11 Not surprisingly, SGen at a minimum supports many of the same operations as Gen, and the implementations
      // are rather mechanical. Define some convenience functions on SGen that simply delegate to the corresponding
      // functions on Gen.
      def flatMap[B](f: A => SGen[B]): SGen[B] = new SGen[B](n => this.forSize(n).flatMap[B](a => f(a).forSize(n)))

      def listOfN(size: SGen[Int]): SGen[List[A]] = new SGen[List[A]](n => this.forSize(n).listOfN(size.forSize(n)))

    }

    // 8.12 Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of a
    // Gen. The implementation should generate lists of the requested size.
    def listOf[A](g: Gen[A]): SGen[List[A]] = new SGen[List[A]](n => g.listOfN(unit(n)))

    // 8.10 Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.

    //     def unsized: SGen[A] = new SGen[A]( n => this)
  }

  object Phase3 {

    import Phase2.{Gen, Passed, Falsified, Result, SGen, TestCases, unit}

    type MaxSize = Int

    case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
      // EXERCISE 8.9
      // Now that we have a representation of Prop, implement && and || for composing Prop values. Notice that in the
      // case of failure we don’t know which property was responsible, the left or the right. Can you devise a way of
      // handling this, perhaps by allowing Prop values to be assigned a tag or label which gets displayed in the event
      // of a failure?
      final def &&(p: Prop): Prop = {
        def newRunner(ms: MaxSize, tc: TestCases, r: RNG): Result = this.run(ms, tc, r) match {
          case Passed => p.run(ms, tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: LET prop passed but RIGHT Prop falsified: " + rFail, rSuc)
            case Passed => Passed
          }
          case Falsified(lFail, lSuc) => p.run(ms, tc, r) match {
            case Passed => new Falsified("Prop.&&: RIGHT Prop passed but LEFT Prop falsified: " + lFail, lSuc)
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props falsified: (LEFT prop after " + lSuc + " test cases: " + lFail + ", RIGHT prop after " + rSuc + " test cases: " + rFail + ")", lSuc.min(rSuc))
          }
        }
        Prop(newRunner)
      }

      final def ||(p: Prop): Prop = {
        def newRunner(ms: MaxSize, tc: TestCases, r: RNG): Result = this.run(ms, tc, r) match {
          case Passed => Passed
          case Falsified(lFail, lSuc) => p.run(ms, tc, r) match {
            case Passed => Passed
            case Falsified(rFail, rSuc) => new Falsified("Prop.||: BOTH Props falsified: (LEFT prop after " + lSuc + " test cases: " + lFail + ", RIGHT prop after " + rSuc + " test cases: " + rFail + ")", lSuc.max(rSuc))
          }
        }
        Prop(newRunner)
      }
    }

    // 2 usefull constants for the type Prop
    def alwaysFalsified: Prop = new Prop((_, _, _) => Falsified("always Falsified", 0))

    def alwaysPassed: Prop = new Prop((_, _, _) => Passed)

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = SimpleRNG(System.currentTimeMillis))
    : String =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) => "Falsified after "+n+" passed tests:\n"+msg
        case Passed => "Passed " +testCases+" tests."
      }


    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

    def forAll[A](g: Int => Phase2.Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val casesPerSize: Int = (n + max) / (max.abs+1)
        val propSequence: Int => Phase2.Prop = i => Phase2.forAll(g(i))(f)
        val propPhase2List: List[Phase2.Prop] = List.map(List.integers(0)(n.min(max)))(propSequence)
        val propPhase3List: List[Prop] = List.map(propPhase2List)(p => Prop {
          (_, _, rng) => p.run(casesPerSize, rng)
        })
        val prop: Prop = List.foldLeft[Prop, Prop](propPhase3List, alwaysPassed)(prop1 => prop2 => prop2.&&(prop1))
        log("...forAll: max="+max+"  n="+n+"    casesPerSize="+casesPerSize+"   propPhase2List.size="+List.length(propPhase2List)+"   propPhase3List.size="+List.length(propPhase3List))
        prop.run(max, n, rng)
    }

    // 8.12 Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of a
    // Gen. The implementation should generate lists of the requested size.
  }

}

object nith_Chapter_08 extends App {

  import Ch08.Phase2.{boolean, choose, forAll, Gen, listOf, listOfN, SGen, union, unit, weighted}
  import Ch08.Phase3.{Prop, run}

  println("****** Chapter_08 ******")
  val rng0: RNG = SimpleRNG(0)
  val rng1: RNG = SimpleRNG(0).nextInt._2
  val simpleRNGiterator: Int => Stream[(Int, RNG)] = count => unfold2[(Int, RNG), RNG](count)(rng0)(rng => {
    val (n, rng2) = rng.nextInt
    ((n, rng2), rng2)
  })


  log("rng0 = %s".format(rng0))
  log("rng1 = %s".format(rng1))
  log("simpleRNGiterator(9) = %s".format(simpleRNGiterator(9).myString))

  println("\n** Exercise 8.4 **")
  log("choose(0,42).sample.run(rng0)                                  = %s".format(choose(0, 42).sample.run(rng0)))
  log("choose(0,42).sample.run(choose(0,42).sample.run(rng0)._2))     = %s".format(choose(0, 42).sample.run(choose(0, 42).sample.run(rng0)._2)))
  log("choose(0.0,42.0).sample.run(rng0)                              = %s".format(choose(0.0, 42.0).sample.run(rng0)))
  log("choose(0.0,42.0).sample.run(choose(0,42).sample.run(rng0)._2)) = %s".format(choose(0.0, 42.0).sample.run(choose(0.0, 42.0).sample.run(rng0)._2)))

  println("\n** Exercise 8.5 **")
  log("unit(\"a\").sample.run(rng0)                          = %s".format(unit("a").sample.run(rng0)))
  log("unit(\"a\").sample.run(unit(\"a\").sample.run(rng0)._2) = %s".format(unit("a").sample.run(unit("a").sample.run(rng0)._2)))
  log("boolean.sample.run(rng0)                            = %s".format(boolean.sample.run(rng0)))
  log("boolean.sample.run(boolean.sample.run(rng0))        = %s".format(boolean.sample.run(boolean.sample.run(rng0)._2)))
  log("listOfN(10,unit(42)).sample.run(rng0)               = %s".format(listOfN(10, unit(42)).sample.run(rng0)))
  log("listOfN[Int](10,choose(0,42)).sample.run(rng0)      = %s".format(listOfN[Int](10, choose(0, 42)).sample.run(rng0)))

  println("\n** Exercise 8.6 **")
  log("unit(\"a\").listOfN(unit(10)).sample.run(rng0)        = %s".format(unit("a").listOfN(unit(10)).sample.run(rng0)))
  log("choose(0,42).listOfN(unit(10)).sample.run(rng0)     = %s".format(choose(0, 42).listOfN(unit(10)).sample.run(rng0)))


  println("\n** Exercise 8.7 and 8.8**")
  log("union(choose(-42,-1),choose(1,42)).listOfN(unit(20)).sample.run(rng0))               = %s".format(union(choose(-42, -1), choose(1, 42)).listOfN(unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.0),(choose(1,42),1.0)).listOfN(unit(20)).sample.run(rng0) = %s".format(weighted((choose(-42, -1), 0.0), (choose(1, 42), 1.0)).listOfN(unit(20)).sample.run(rng0)))
  //  log("unfold(rng0)(rng => Some(double(rng))).take(20)                                      = %s".format(unfold[Double, RNG](rng0)(rng => Some(double(rng))).take(20).myString))
  log("weighted((choose(-42,-1),0.1),(choose(1,42),0.9)).listOfN(unit(20)).sample.run(rng0) = %s".format(weighted((choose(-42, -1), 0.1), (choose(1, 42), 0.9)).listOfN(unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.2),(choose(1,42),0.8)).listOfN(unit(20)).sample.run(rng0) = %s".format(weighted((choose(-42, -1), 0.2), (choose(1, 42), 0.8)).listOfN(unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.5),(choose(1,42),0.5)).listOfN(unit(20)).sample.run(rng0) = %s".format(weighted((choose(-42, -1), 0.5), (choose(1, 42), 0.5)).listOfN(unit(20)).sample.run(rng0)))

  println("\n** Exercise 8.9 **")
  log("choose(1,100).listOfN(unit(10)).sample.run(rng0)      = %s".format(choose(1, 100).listOfN(unit(10)).sample.run(rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0)         = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).run(10, rng0)))
  log("choose(1,100))(n => n MOD 7 > 0).run(10,rng0)         = %s".format(forAll[Int](choose(1, 100))(n => n % 7 > 0).run(10, rng0)))
  log("conjunction &&                                        = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).&&(forAll[Int](choose(1, 100))(n => n % 7 > 0)).run(10, rng0)))
  log("disjunction ||                                        = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).||(forAll[Int](choose(1, 100))(n => n % 7 > 0)).run(10, rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0) && true = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).&&(forAll[Int](unit(0))(_ => true)).run(10, rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0) || true = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).||(forAll[Int](unit(0))(_ => true)).run(10, rng0)))

  println("\n** Exercise 8.12 **")
  log("listOf(unit(\"a\")).forSize(0).sample.run(rng0)    = %s".format(listOf(unit("a")).forSize(0).sample.run(rng0)))
  log("listOf(choose(0,42)).forSize(0).sample.run(rng0) = %s".format(listOf(choose(0, 42)).forSize(0).sample.run(rng0)))
  log("listOf(unit(\"a\")).forSize(4).sample.run(rng0)    = %s".format(listOf(unit("a")).forSize(4).sample.run(rng0)))
  log("listOf(choose(0,42)).forSize(4).sample.run(rng0) = %s".format(listOf(choose(0, 42)).forSize(4).sample.run(rng0)))
  log("listOf(unit(\"a\")).forSize(8).sample.run(rng0)    = %s".format(listOf(unit("a")).forSize(8).sample.run(rng0)))
  log("listOf(choose(0,42)).forSize(8).sample.run(rng0) = %s".format(listOf(choose(0, 42)).forSize(8).sample.run(rng0)))

  println("\n** Exercise 8.13 **")
  val smallInt: Gen[Int] = choose(-10, 10)
  val maxIsTheBiggest: List[Int] => Boolean = (ns: List[Int]) => {
    val max: Int = List.max(ns)
    !List.exists(ns)(_ > max)
  }
  val maxProp : Prop = Ch08.Phase3.forAll[List[Int]](listOf(smallInt))(maxIsTheBiggest)
  log("maxIsTheBiggest(List.Nil) = " + maxIsTheBiggest(List.Nil))
  log("run(maxProp) = " + run(maxProp))

  println("\n** Exercise 8.13 **")
  println("** Write a property to verify the behavior of List.sorted ...")
  log("run(forAll[List[Int]](listOf(smallInt))(List.isSorted),0) = " + run(Ch08.Phase3.forAll[List[Int]](listOf(smallInt))(List.isSorted),0))
  log("run(forAll[List[Int]](listOf(smallInt))(List.isSorted),1) = " + run(Ch08.Phase3.forAll[List[Int]](listOf(smallInt))(List.isSorted),1))
  log("run(forAll[List[Int]](listOf(smallInt))(List.isSorted),1) = " + run(Ch08.Phase3.forAll[List[Int]](listOf(smallInt))(List.isSorted),10))
  log("run(forAll[List[Int]](listOf(smallInt))(l=>List.isSorted(List.mergeSort(l))) = " + run(Ch08.Phase3.forAll[List[Int]](listOf(smallInt))(l => List.isSorted(List.mergeSort(l)))))
//  log("run(forAll[List[Int]](listOf(smallInt))(l=>List.isSorted(List.mergeSort(l))) = " + run(Ch08.Phase3.forAll[List[Int]](listOf(smallInt))(l => mergeSortPar[Int](l)(List.mergeSortPar(l)))))

//  mergeSortPar[Int](List.integers(99)(0))(n => m => n < m)(esUnlimited).get))


  println("*** Not finished yet ***")

}

