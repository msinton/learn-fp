package ch.five

object Exercises {

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
