package org.dka.tutorial.fp

trait NumberGenerator {
  type NextGen[+A] = (A, NumberGenerator)
  type Rand[+A] = NumberGenerator => NextGen[A]

  // --------------------------------------------------------
  // what must be implemented
  // --------------------------------------------------------
  def nextInt: NextGen[Int]

  // --------------------------------------------------------
  // other methods (based on nextInt)
  // --------------------------------------------------------
  def nonNegativeInt(generator: NumberGenerator): NextGen[Int] = next[Int](generator)(i => if (i < 0) -(i+1) else i)

  def nextBoolean(generator: NumberGenerator): NextGen[Boolean] = next[Boolean](generator)(i => i % 2 == 0)

  def nextDouble(generator: NumberGenerator): NextGen[Double] = next[Double](generator)(i => i / (Int.MaxValue.toDouble + 1))

  def nextInts(size: Int): NextGen[List[Int]] = nextList[Int](size)(g => g.nextInt)

  def nextNonNegativeInts(size: Int): NextGen[List[Int]] = nextList[Int](size)(g => g.nonNegativeInt(g))

  def nextBooleans(size: Int): NextGen[List[Boolean]] = nextList[Boolean](size)(g => g.nextBoolean(g))

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def nextDie(size: Int): NextGen[List[Int]] = nextList[Int](size)(g => g.rollDie(g))

  // --------------------------------------------------------
  // function building blocks
  // --------------------------------------------------------
  private def next[A](generator: NumberGenerator)(f: Int => A): NextGen[A] = map[Int, A](_.nextInt)(f).apply(generator)

  private def nextList[A](size: Int)(f: NumberGenerator => NextGen[A]): NextGen[List[A]] =
    (1 to size)
      .foldLeft( (List[A](), this) ) ( (e, _) => {
                                                   val (r, g) = f(e._2)
                                                   (r :: e._1, g)
                                                 }
                                       )

  def map[A,B](f:Rand[A])(g: A => B): Rand[B] = flatMap(f)(a => unit(g(a)))
//  def map[S,A,B](f: S => (A,S))(g: A => B): S => (B,S) = //flatMap(f)(a => unit(g(a)))
//    g1 => {
//      val (a, g2) = f(g1)
//      (g(a), g2)
//    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    g1 => {
      val (r, g2) = f(g1)
      g(r)(g2) // We pass the new state along
    }

  def unit[A](r:A): Rand[A] = g => (r, g)
}


case class SimpleNumberGenerator(seed: Long) extends NumberGenerator {
  def nextInt: (Int, NumberGenerator) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val generator = SimpleNumberGenerator(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, generator)
  }
}
