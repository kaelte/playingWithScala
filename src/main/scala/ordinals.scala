package ordinals

import scala.annotation.tailrec


sealed trait Ordinal {

  final override def toString: String = {
    // a beautiful string
    if (this.isFinite)
      this.toBigInt.get.toString
    else this match {
      case Ord(ords) => Ordinal.ordListToString(ords)
    }
  }

  final def isZero: Boolean = this match {
      // returns true iff this is zero
    case Ord(List()) => true
    case Ord(ords) => false
  }

  final def isFinite: Boolean = this match {
    // returns true iff this is finite
    case Ord(ords) => ords.forall(_.isZero)
  }

  final def toBigInt: Option[BigInt] = {
    // returns Some(i) if this is the finite ordinal i
    // None otherwise
    if (this.isFinite)
      this match {
        case Ord(ords) => Option(ords.length)
      }
    else None
  }

  final def isSmaller(beta: Ordinal): Boolean = this match {
    // returns true iff this is smaller than that
    case Ord(List()) => !(beta == Ordinal.zero)
    case Ord(List(alphas)) => true
  }

}

//case object Nil extends Ordinal
case class Ord(ords: List[Ordinal]) extends Ordinal


object Ordinal {
  final val zero: Ordinal = Ord(List())
  final val one: Ordinal = Ord(List(zero))
  final val omega: Ordinal = Ord(List(one))
  final val omegaStr: String = "W"


  final def ordListToString(ords: List[Ordinal]): String = ords match {
    case List() => "NIL"
    case o :: List() => omegaStr + "^(" + o + ")"
    case o :: os => omegaStr + "^(" + o + ") + " + ordListToString(os)
  }

  final def apply(i: Int):Ordinal = Ord(List().padTo(i.max(0),zero))

}


object ordinalApp extends App {

  println("****************************")
  println("********* Ordinals *********")
  println("****************************")
  println("System.getProperty(\"file.encoding\") = " + System.getProperty("file.encoding"))
  println(s"Hellenic letters : α β γ ω")
  println("****************************")
  println("Ordinal.zero           = " + Ordinal.zero)
  println("Ordinal.zero.isZero    = " + Ordinal.zero.isZero)
  println("Ordinal.zero.isFinite  = " + Ordinal.zero.isFinite)
  println("Ordinal(1)             = " + Ordinal(1))
  println("Ordinal(1).isZero      = " + Ordinal(1).isZero)
  println("Ordinal(1).isFinite    = " + Ordinal(1).isFinite)
  println("Ordinal(42)            = " + Ordinal(42))
  println("Ordinal(42).isZero     = " + Ordinal(42).isZero)
  println("Ordinal(42).isFinite   = " + Ordinal(42).isFinite)
  println("Ordinal(-42)           = " + Ordinal(-42))
  println("Ordinal(-42).isZero    = " + Ordinal(-42).isZero)
  println("Ordinal(-42).isFinite  = " + Ordinal(-42).isFinite)
  println("Ordinal.omega          = " + Ordinal.omega)
  println("Ordinal.omega.isZero   = " + Ordinal.omega.isZero)
  println("Ordinal.omega.isFinite = " + Ordinal.omega.isFinite)
  println("****************************")
  println("***** Work in Progress *****")
  println("Ordinal(0).isSmaller(Ordinal(0)) = " + Ordinal(0).isSmaller(Ordinal(0)))
  println("Ordinal(0).isSmaller(Ordinal(1)) = " + Ordinal(0).isSmaller(Ordinal(1)))
  println("Ordinal(1).isSmaller(Ordinal(0)) = " + Ordinal(1).isSmaller(Ordinal(0)))
  println("Ordinal(1).isSmaller(Ordinal(1)) = " + Ordinal(1).isSmaller(Ordinal(1)))
  println("****************************")
  println("***** Work in Progress *****")
  println("****************************")
  println("Test : " + -42.max(0))
}

/// Local Variables:
/// mode: scala
/// coding: utf-8
/// End:
