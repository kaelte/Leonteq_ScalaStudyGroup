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

    trait Prop {
      def check: Either[(FailedCase, SuccessCount), SuccessCount]
    }

    case class Gen[A](sample: Ch06.State[Ch06.RNG, A]) {
      // 8.6 Implement flatMap, and then use it to implement this more dynamic version of listOfN.
      def flatMap[B](f: A => Gen[B]): Gen[B] = new Gen[B](this.sample.flatMap[B](f(_).sample))

      //      def listOfN(size: Gen[Int]): Gen[List[A]] = flatMap[List[A]](a => Ch06.sequence[Ch06.RNG, A](List.fill[Ch06.RandState[A]](size.sample.run)(???)))
      def listOfN(size: Gen[Int]): Gen[List[A]] = {
        val runOfAs : Ch06.RNG => (List[A],Ch06.RNG) = rng => Ch06.sequence[Ch06.RNG, A](List.fill[Ch06.RandState[A]](size.sample.run(rng)._1)(this.sample)).run(rng)
        val stateOfAs: Ch06.State[Ch06.RNG, List[A]] = new Ch06.State[Ch06.RNG, List[A]](runOfAs)
        new Gen[List[A]](stateOfAs)
      }
    }

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

    // 8.4 Implement Gen.choose using this representation of Gen. It should generate integers in the range
    // start to stopExclusive. Feel free to use functions you’ve already written.
    def choose(start: Int, stopExclusive: Int): Gen[Int] = new Gen[Int](Ch06.nonNegativeLessThanState(stopExclusive - start).map[Int](_ + start))
    final def choose(start: Double, stopExclusive: Double): Gen[Double] = new Gen[Double](Ch06.doubleRandState.map[Double]( x=>(stopExclusive - start)*x+start))

    // 8.5 Let’s see what else we can implement using this representation of Gen.
    // Try implementing unit, boolean, and listOfN.
    final def unit[A](a: => A): Gen[A] = new Gen[A](Ch06.unitState[Ch06.RNG, A](a))
    final def boolean: Gen[Boolean] = new Gen[Boolean](new Ch06.State[Ch06.RNG, Boolean](rng => rng.nextInt match {
      case (n, r) => (n % 2 == 0, r)
    }))
    final def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val stateOfAs: Ch06.State[Ch06.RNG, List[A]] = Ch06.sequence[Ch06.RNG, A](List.fill[Ch06.RandState[A]](n)(g.sample))
      new Gen[List[A]](stateOfAs)
    }


    // 8.7 Implement union, for combining two generators of the same type into one,
    // by pulling values from each generator with equal likelihood.
    final def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap[A](p => if (p) g1 else g2)
    final def unionLazy[A](g1: => Gen[A], g2: => Gen[A]): Gen[A] = boolean.flatMap[A](p => if (p) g1 else g2)

    // 8.8 Implement weighted, a version of union that accepts a weight for each Gen and generates values
    // from each Gen with probability proportional to its weight.
    final def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = choose(0.0-g1._2, g2._2).flatMap[A](x => if (x<0) g1._1 else g2._1)
    final def weighted2[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val g1weight : Double = g1._2 / (g1._2 + g2._2)
      val doubleGen : Gen[Double] = new Gen[Double](Ch06.doubleRandState)
      util.log("...weighted : g1weight="+g1weight) 
      doubleGen.flatMap[A]((x => if (x<g1weight) g1._1 else g2._1))
      }
  }


}

object nith_Chapter_08 extends App {

  println("****** Chapter_08 ******")
  val rng0: Ch06.RNG = Ch06.SimpleRNG(0)
  val rng1: Ch06.RNG = Ch06.SimpleRNG(0).nextInt._2
  val simpleRNGiterator: Int => Ch05.Stream[(Int, Ch06.RNG)] = count => Ch05.unfold2[(Int, Ch06.RNG), Ch06.RNG](count)(rng0)(rng => {
    val (n, rng2) = rng.nextInt
    ((n, rng2), rng2)
  })


  util.log("rng0 = %s".format(rng0))
  util.log("rng1 = %s".format(rng1))
  util.log("simpleRNGiterator(9) = %s".format(simpleRNGiterator(9).myString))

  println("\n** Exercise 8.4 **")
  util.log("choose(0,42).sample.run(rng0)                                  = %s".format(Ch08.Phase2.choose(0,42).sample.run(rng0)))
  util.log("choose(0,42).sample.run(choose(0,42).sample.run(rng0)._2))     = %s".format(Ch08.Phase2.choose(0,42).sample.run(Ch08.Phase2.choose(0,42).sample.run(rng0)._2)))
  util.log("choose(0.0,42.0).sample.run(rng0)                              = %s".format(Ch08.Phase2.choose(0.0,42.0).sample.run(rng0)))
  util.log("choose(0.0,42.0).sample.run(choose(0,42).sample.run(rng0)._2)) = %s".format(Ch08.Phase2.choose(0.0,42.0).sample.run(Ch08.Phase2.choose(0.0,42.0).sample.run(rng0)._2)))

  println("\n** Exercise 8.5 **")
  util.log("unit(\"a\").sample.run(rng0)                          = %s".format(Ch08.Phase2.unit("a").sample.run(rng0)))
  util.log("unit(\"a\").sample.run(unit(\"a\").sample.run(rng0)._2) = %s".format(Ch08.Phase2.unit("a").sample.run(Ch08.Phase2.unit("a").sample.run(rng0)._2)))
  util.log("boolean.sample.run(rng0)                            = %s".format(Ch08.Phase2.boolean.sample.run(rng0)))
  util.log("boolean.sample.run(boolean.sample.run(rng0))        = %s".format(Ch08.Phase2.boolean.sample.run(Ch08.Phase2.boolean.sample.run(rng0)._2)))
  util.log("listOfN(10,unit(42)).sample.run(rng0)               = %s".format(Ch08.Phase2.listOfN(10,Ch08.Phase2.unit(42)).sample.run(rng0)))
  util.log("listOfN[Int](10,choose(0,42)).sample.run(rng0)      = %s".format(Ch08.Phase2.listOfN[Int](10,Ch08.Phase2.choose(0,42)).sample.run(rng0)))

  println("\n** Exercise 8.6 **")
  util.log("unit(\"a\").listOfN(unit(10)).sample.run(rng0)        = %s".format(Ch08.Phase2.unit("a").listOfN(Ch08.Phase2.unit(10)).sample.run(rng0)))


  println("\n** Exercise 8.7 and 8.8**")
  util.log("union(choose(-42,-1),choose(1,42)).listOfN(unit(20)).sample.run(rng0))               = %s".format(Ch08.Phase2.union(Ch08.Phase2.choose(-42,-1),Ch08.Phase2.choose(1,42)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))
  util.log("weighted((choose(-42,-1),0.0),(choose(1,42),1.0)).listOfN(unit(20)).sample.run(rng0) = %s".format(Ch08.Phase2.weighted((Ch08.Phase2.choose(-42,-1),0.0),(Ch08.Phase2.choose(1,42),1.0)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))
  util.log("unfold(rng0)(rng => Some(double(rng))).take(20)                                      = %s".format(Ch05.unfold[Double, Ch06.RNG](rng0)(rng => Some(Ch06.double(rng))).take(20).myString))
  util.log("weighted((choose(-42,-1),0.1),(choose(1,42),0.9)).listOfN(unit(20)).sample.run(rng0) = %s".format(Ch08.Phase2.weighted((Ch08.Phase2.choose(-42,-1),0.1),(Ch08.Phase2.choose(1,42),0.9)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))
  util.log("weighted((choose(-42,-1),0.2),(choose(1,42),0.8)).listOfN(unit(20)).sample.run(rng0) = %s".format(Ch08.Phase2.weighted((Ch08.Phase2.choose(-42,-1),0.2),(Ch08.Phase2.choose(1,42),0.8)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))
  util.log("weighted((choose(-42,-1),0.5),(choose(1,42),0.5)).listOfN(unit(20)).sample.run(rng0) = %s".format(Ch08.Phase2.weighted((Ch08.Phase2.choose(-42,-1),0.5),(Ch08.Phase2.choose(1,42),0.5)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))

  println("*** Not finished yet ***")

}

