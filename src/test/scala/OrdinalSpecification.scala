import ordinals.Ordinal._
import ordinals.{Ord, Ordinal}
import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop._


object OrdinalSpecification extends Properties("Ordinal") {

  val integerGenerator: Gen[Int] = Gen.choose(0,128)
  val ordinalGenerator: Gen[Ord] = Gen.choose(1,9).flatMap(generate)

  property("integerFiniteOrdinal") = forAll[Int,Boolean](integerGenerator)(n => (n == Ordinal(n).toInt.get))

  /// it would be good to reduce the number of test cases but use more complex ones
  /// ??? How to reduced the number of test cases ???
  property(" (α=0) ∨ isSuccessor(α) ∨ isLimit(α)") = {
    forAll[Ord,Boolean](ordinalGenerator)(alpha => (alpha.isZero || alpha.isSuccessor || alpha.isLimit))
  }

  property(" α = predecessor(successor(α))") = {
    forAll[Ord,Boolean](ordinalGenerator)(alpha => alpha == alpha.successor.predecessor.get)
  }

  property(" isNormal(α.normalise)") = {
    forAll[Ord,Boolean](ordinalGenerator)(alpha => alpha.normalise.isNormal)
  }

  /// Addition .add
  property(" α+0 = α") = {
    forAll[Ord,Boolean](ordinalGenerator)(alpha => alpha.add(zero) == alpha)
  }

  property(" α+1 = successor(α)") = {
    forAll[Ord,Boolean](ordinalGenerator)(alpha => alpha.add(one) == alpha.successor)
  }

  property(" α < β  => γ+α < γ+β ") = {
    forAll[Ord,Ord,Ord,Boolean](ordinalGenerator,ordinalGenerator,ordinalGenerator)({
      (alpha,beta,gamma) => (-1 < alpha.compare(beta) || -1 == gamma.add(alpha).compare(gamma.add(beta)))
    })
  }

  /// Multiplication .mult
  property(" α·0 = 0") = {
    forAll[Ord,Boolean](ordinalGenerator)(alpha => alpha.mult(zero) == zero)
  }

  property(" α·successor(β) = (α·β)+α") = {forAll[Ord,Ord,Boolean](ordinalGenerator,ordinalGenerator)((alpha,beta)
  => alpha.mult(beta.successor) == alpha.mult(beta).add(alpha))
  }

  property(" α < β => 0 < γ => γ·α < γ·β ") = {
    forAll[Ord,Ord,Ord,Boolean](ordinalGenerator,ordinalGenerator,ordinalGenerator)({
      (alpha,beta,gamma) => (-1 < alpha.compare(beta) || zero == gamma || -1 == gamma.mult(alpha).compare(gamma.mult(beta)))
    })
  }

  /// Distributivity
  property(" α·(β+γ) = α·β+α·γ") = {
    forAll[Ord,Ord,Ord,Boolean](ordinalGenerator,ordinalGenerator,ordinalGenerator)({
      (alpha,beta,gamma) => alpha.mult(beta.add(gamma)) == alpha.mult(beta).add(alpha.mult(gamma))
    })
  }

}