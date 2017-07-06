package org.dka.tutorial.fp

import org.scalatest.{FunSpec, Matchers}

class SimpleNumberGeneratorSpec extends FunSpec with Matchers {
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
      result shouldBe result2
    }
    it ("should generate different lists of booleans") {
      val rng1:NumberGenerator = SimpleNumberGenerator(42)

      val (result, generator) = rng1.nextBooleans(10)
      val (result2, generator2) = generator.nextBooleans(10)
      result should not be result2
    }
  }

}
