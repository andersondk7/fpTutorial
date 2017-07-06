package org.dka.tutorial.fp


trait NumberGenerator {
  type Next[+A] = (A, NumberGenerator)
  type Rand[+A] = NumberGenerator => Next[A]

  // what must be implemented
  def nextInt: Next[Int]

  // other methods (based on nextInt)
  def nonNegativeInt(generator: NumberGenerator): Next[Int] =
    map(_.nextInt)(i => if (i < 0) -(i+1) else i).apply(this)

  def nextBoolean(generator: NumberGenerator): Next[Boolean] =
    map(_.nextInt)(i => i % 2 == 0).apply(this)

  def nextDouble(generator: NumberGenerator): Next[Double] =
    map(_.nextInt)(i => i / (Int.MaxValue.toDouble + 1)).apply(this)

  def nextInts(size: Int): Next[List[Int]] = nextList[Int](size)(g => g.nextInt)

  def nextNonNegativeInts(size: Int): Next[List[Int]] = nextList[Int](size)(g => g.nonNegativeInt(g))

  def nextBooleans(size: Int): Next[List[Boolean]] = nextList[Boolean](size)(g => g.nextBoolean(g))


  // function building blocks
  private def nextList[A](size: Int)(f: NumberGenerator => Next[A]): Next[List[A]] =
    (1 to size)
      .foldLeft( (List[A](), this) ) ( (e, _) => {
                                                   val (r, g) = f(e._2)
                                                   (r :: e._1, g)
                                                 }
                                       )
//  private def unit[A](a:A): Rand[A] = g => (a, g)
  private def map[A,B](s:Rand[A])(f: A => B): Rand[B] =
    g => {
      val (a, g2) = s(g)
      (f(a), g2)
    }
}


case class SimpleNumberGenerator(seed: Long) extends NumberGenerator {
  def nextInt: (Int, NumberGenerator) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val generator = SimpleNumberGenerator(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, generator)
  }
}
