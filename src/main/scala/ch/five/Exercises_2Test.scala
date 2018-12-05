package ch.five
import org.scalacheck.Prop._
import org.scalacheck.Properties

object Exercises_2Test extends Properties("non-negative even int") {

  property("always divisible by 2") = forAll {i: Long =>
    val rng = SimpleRNG(i)
    val (result, _) = Exercises_2.nonNegativeEven(rng)
    result % 2 == 0
  }

  property("always non negative") = forAll {i: Long =>
    val rng = SimpleRNG(i)
    val (result, _) = Exercises_2.nonNegativeEven(rng)
    result >= 0
  }
}
