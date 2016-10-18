package ordinals

import scala.annotation.tailrec
import scala.collection.immutable.Vector
import scala.collection.parallel.immutable._
import Ordinal._

sealed trait Ordinal {

  final def getExpList: ParVector[Ordinal] = this match {case Ord(ords) => ords}

  final override def toString: String = {
    // a beautiful string
    if (this.isFinite)
      this.toBigInt.get.toString
    else Ordinal.ordVecToString(this.getExpList)
  }

  final def isZero: Boolean = (this == zero)

  final def isFinite: Boolean = this.getExpList.forall(_.isZero)

  final def toBigInt: Option[BigInt] = if (this.isFinite) Some(this.getExpList.length) else None
  // toBigInt returns Some(i) iff this is the finite ordinal i

  final def isSmaller(beta: Ordinal): Boolean = {
    // returns true iff this is smaller than that
    if (this.isFinite)
      if (beta.isFinite)
        this.toBigInt.get < beta.toBigInt.get
      else true
    // work in progress !!!    
      else {
        println("isSmaller:  !!!! WORK IN PROGRESS: Do not trust the result !!!!")
        if (beta.isFinite) false else true
        }
  }
}

case class Ord(ords: ParVector[Ordinal]) extends Ordinal

object Ordinal {
  final val zero: Ordinal = Ord(Vector().par)
  final val one: Ordinal = Ord(Vector(zero).par)
  final val omega: Ordinal = Ord(Vector(one).par)
  final val omegaStr: String = "W"

  final def apply(i: Int): Ordinal = Ord(Vector.fill(i.max(0))(zero).par)

  final def ordVecToString(ords: ParVector[Ordinal]): String = {
    @tailrec
    def go(ords: ParVector[Ordinal], str: String): String = {
      if (ords.isEmpty) str
      else {
        val ordsHead: Ordinal = ords.head
        val headExponent: String = {
          if (ordsHead == one) ""
          else {
            val ordsHeadStr: String = ordsHead.toString
            if (ordsHeadStr.length == 1) "^" + ordsHeadStr else "^(" + ordsHeadStr + ")"
          }
        }
        if (ords.length == 1) str + omegaStr + headExponent
        else go(ords.tail, str + omegaStr + headExponent)
      }
    }
    go(ords, "")
  }

  final def omegaStack(n: BigInt): Ordinal = {
    @tailrec
    def go(i: BigInt, alpha: Ordinal): Ordinal = {
      if (i < 1) alpha
      else go(i - 1, Ord(Vector(alpha).par))
    }
    go(n, one)
  }

}

object ordinalApp extends App {

  import util._
  logg("System.getProperty(\"file.encoding\")")(System.getProperty("file.encoding"))
  logg("Hellenic letters")("α β γ ω")
  println("****************************")
  println("********* Ordinals *********")
  println("****************************")
  println("zero                  = " + zero)
  println("zero.isZero           = " + zero.isZero)
  println("zero.isFinite         = " + zero.isFinite)
  println("Ordinal(1)            = " + Ordinal(1))
  println("Ordinal(1).isZero     = " + Ordinal(1).isZero)
  println("Ordinal(1).isFinite   = " + Ordinal(1).isFinite)
  println("Ordinal(42)           = " + Ordinal(42))
  println("Ordinal(42).isZero    = " + Ordinal(42).isZero)
  println("Ordinal(42).isFinite  = " + Ordinal(42).isFinite)
  println("Ordinal(-42)          = " + Ordinal(-42))
  println("Ordinal(-42).isZero   = " + Ordinal(-42).isZero)
  println("Ordinal(-42).isFinite = " + Ordinal(-42).isFinite)
  println("omega                 = " + omega)
  println("omega.isZero          = " + omega.isZero)
  println("omega.isFinite        = " + omega.isFinite)
  println("omega^(42)            = " + Ord(Vector(Ordinal(42)).par))
  println("omega^(42).isZero     = " + Ord(Vector(Ordinal(42)).par).isZero)
  println("omega^(42).isFinite   = " + Ord(Vector(Ordinal(42)).par).isFinite)
  println("****************************")
  logg("omegaStack(-42)")(omegaStack(-42))
  logg("omegaStack(0)")(omegaStack(0))
  logg("omegaStack(1)")(omegaStack(1))
  logg("omegaStack(2)")(omegaStack(2))
  logg("omegaStack(3)")(omegaStack(3))
  logg("omegaStack(42)")(omegaStack(42))
  println("****************************")
  logg(Ordinal(0) + ".isSmaller(" + Ordinal(0) + ")")(Ordinal(0).isSmaller(Ordinal(0)))
  logg(Ordinal(0) + ".isSmaller(" + Ordinal(42) + ")")(Ordinal(0).isSmaller(Ordinal(42)))
  logg(Ordinal(42) + ".isSmaller(" + Ordinal(0) + ")")(Ordinal(42).isSmaller(Ordinal(0)))
  logg(Ordinal(42) + ".isSmaller(" + Ordinal(42) + ")")(Ordinal(42).isSmaller(Ordinal(42)))
  logg(Ordinal(42) + ".isSmaller(" + omega + ")")(Ordinal(42).isSmaller(omega))
  logg(omega + ".isSmaller(" + omega + ")")(omega.isSmaller(omega))
  println("****************************")
  println("***** Work in Progress *****")
  println("****************************")
  println("Test : " + -42.max(0))
}

/// Local Variables:
/// mode: scala
/// coding: utf-8
/// End:
