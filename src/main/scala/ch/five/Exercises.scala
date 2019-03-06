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
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
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

  val nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))

  def int: Rand[Int] = rng => rng.nextInt

  def double: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1))

  // we need this for intDouble, doubleInt etc
  def map2[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = sa(rng)
      val (b, rng3) = sb(rng2)
      (f(a, b), rng3)
    }

  def doubleInt: Rand[(Double, Int)] =
    both(double, int)

  def intDouble: Rand[(Int, Double)] =
    both(int, double)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](sas: List[Rand[A]]): Rand[List[A]] =
    rng => sas.foldLeft((List.empty[A], rng)) {
      case ((xs, nextRng), sa) =>
        val (a, rng2) = sa(nextRng)
        (xs :+ a, rng2)
    }



  // TODO test

  // if we provide 10, it should return a random number 0-9 (less than n)
   def nonNegativeLessThan(n:Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      a => rng => {
        val mod = a % n
        if (a + (n - 1) - mod >= 0)
          (mod, rng)
        else
         nonNegativeLessThan(n)(rng)
      }
    }
  }


  def flatMap[A,B](r: Rand[A])(f: A => Rand[B]): Rand[B]={
    rng => {
      val (a, rng2) = r(rng)
      f(a)(rng2)
    }
  }

  // TODO test/check
  //  def sequence2[A](sas: List[Rand[A]]): Rand[List[A]] = rng =>
//
  // use sequence to implement ints, hint use List.fill
  def ints(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(int))



}
