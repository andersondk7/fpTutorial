package org.dka.tutorial.fp

import org.scalatest.{FunSpec, Matchers}

import scala.collection.immutable.ListMap

class SimpleNumberGeneratorSpec extends FunSpec with Matchers {
  def distribution(list: List[Int]): Map[Int, Int] = {
    val dist: Map[Int, Int] = list.foldLeft(Map[Int, Int]())((m, r) => {
      val count = m.getOrElse(r, 0) + 1
      m + (r -> count)
    })
    ListMap(dist.toSeq.sortBy(_._1): _*)
  }

  describe("SimpleRNGSpec") {
    it("should generate the same random number"){
      val rng1 = SimpleNumberGenerator(42)
      val (r1, _) = rng1.nextInt // don't care about the next generator
      val (r2, _) = rng1.nextInt // don't care about the next generator
      r1 shouldBe r2
    }
    it ("should generate different random numbers with each rng") {
      val rng1 = SimpleNumberGenerator(42)
      val (r1, rng2) = rng1.nextInt // need the next generator to get different random numbers
      val (r2, rng3) = rng2.nextInt // need the next generator to get different random numbers
      r1 should not be r2
    }
    it ("should generate a list of random numbers") {
      val rng1:NumberGenerator = SimpleNumberGenerator(42)

      val (result, generator) = rng1.nextInts(10)
      val (result2, generator2) = rng1.nextInts(10)
      result shouldBe result2
    }
    it ("should generate different lists random numbers") {
      val rng1:NumberGenerator = SimpleNumberGenerator(42)

      val (result, generator) = rng1.nextInts(10)
      val (result2, generator2) = generator.nextInts(10)
      result should not be result2
    }
    it(s"should generate a non-negative integers") {
      val rng1:NumberGenerator = SimpleNumberGenerator(42)
      val (r, g) = rng1.nextNonNegativeInts(10)
//      val (r, g) = rng1.nextInts(10)
      val result: Boolean = r.foldLeft(true) ( (isPos, i) => {
        println(s"i: $i")
        isPos & (i >= 0)
      })
      result shouldBe true
    }
    it ("should generate a consistent list of booleans") {
      val rng1:NumberGenerator = SimpleNumberGenerator(42)

      val (result, generator) = rng1.nextBooleans(10)
      val (result2, generator2) = rng1.nextBooleans(10)
      println(result)
      result shouldBe result2
    }
    it ("should generate different lists of booleans") {
      val rng1:NumberGenerator = SimpleNumberGenerator(42)

      val (result, generator) = rng1.nextBooleans(10)
      val (result2, generator2) = generator.nextBooleans(10)
      result should not be result2
    }
    it ("should roll a die with same random result") {
      val rng1: NumberGenerator = SimpleNumberGenerator(5)
      val (result, generator) = rng1.nextDie(20)
      val sorted: Map[Int, Int] = distribution(result)
      val expected = Map(1 -> 3, 2 -> 2, 3 -> 5, 4 -> 3, 5 ->3, 6 -> 4)
      println(s"distribution: $sorted")
      sorted shouldBe expected
    }
    it ("should roll a die with different random result") {
      val count = 200
      val rng1: NumberGenerator = SimpleNumberGenerator(5)
      val (result, generator) = rng1.nextDie(count)
      val d1: Map[Int, Int] = distribution(result)

      val (result2, _) = generator.nextDie(count)
      val d2 = distribution(result2)
      println(s"d1: $d1")
      println(s"d2: $d2")
      d1 should not be d2
    }
  }

}
