package ch.five

trait RNG {
  def nextInt: (Int, RNG)
}
