package ch.five

object Exercises_1 {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i < 0) (math.abs(i + 1), rng2)
    else (i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng2) = nonNegativeInt(rng)
    (i1.toDouble / (Int.MaxValue + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
//    val (i, rng2) = rng.nextInt
    val a = rng.nextInt
    val b = double(a._2)
    ((a._1, b._1), b._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((List.empty[Int], rng)) {
      case ((xs, nextRng), _) =>
        val (i, rng2) = nextRng.nextInt
        (xs :+ i, rng2)
    }
  }

}

object Exercises_2 {

  import Exercises_1._

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = ???

  def double: Rand[Double] = ???

  // we need this for intDouble, doubleInt etc
  def map2[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def doubleInt: Rand[(Double, Int)] = ???

  def intDouble: Rand[(Int, Double)] = ???

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ???

  def sequence[A](sas: List[Rand[A]]): Rand[List[A]] = ???

  // use sequence to implement ints, hint use List.fill

}
