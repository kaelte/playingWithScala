package ordinals

import scala.annotation.tailrec
import scala.collection.immutable.Vector
import scala.collection.parallel.immutable._
import Ordinal._
import util._


sealed trait Ordinal

case class Ord(ords: ParVector[Ord]) extends Ordered[Ord]  {
  final def getExpList: ParVector[Ord] = this match {case Ord(ords) => ords}

  final override def toString: String = {
    // a beautiful string
    if (this.isFinite) this.toBigInt.get.toString else {
      val expStrs: ParVector[String] = this.getExpList.map(_.toString).map(Ordinal.omegaExpString)
      expStrs.foldLeft("")((x,y) => if (x.isEmpty) y else x + "+" + y)
    }
  }

  final def compare(that: Ord) = {
    if (this.isFinite)
    // if both ordinals finite use bigInt comparison
    // else finite is smaller than infinite
      if (that.isFinite) this.toBigInt.get compare that.toBigInt.get else -1
    else {
    // this infinite that finite => 1
      if (that.isFinite) 1 else {
        @tailrec
        def go(aN:ParVector[Ord],bN:ParVector[Ord]):Int = {
          //go must be called with exponents of normalised ordinals
          (aN.isEmpty,bN.isEmpty) match {
            case(true,true) => 0
            case(true,false) => -1
            case(false,true) => 1
            case(false,false) => {
              val headComp: Int = aN.head.compare(bN.head)
              if (0==headComp) go(aN.tail,bN.tail) else headComp
            }
          }
        }
        // we call go with normalised (this,that)
        go(this.normalise.getExpList,that.normalise.getExpList)
      }
    }
  }
  
  final def isZero: Boolean = (this == zero)

  final def isFinite: Boolean = this.getExpList.forall(_.isZero)

  final def toBigInt: Option[BigInt] = if (this.isFinite) Some(this.getExpList.length) else None
  // toBigInt returns Some(i) iff this is the finite ordinal i

  final def normalise: Ord = {
    if (this.isFinite) this else {
      Ord(ordVecSortedSubVec(this.getExpList).map(_.normalise))
    }
  }

  final def isNormal: Boolean = {
    if (this.isFinite) true
    else {
      val exps = this.getExpList
      ordVecIsSorted(exps) && exps.forall(_.isNormal)
    }
  }

  final def add(that: Ord): Ord = Ord(this.getExpList++that.getExpList).normalise
}



object Ordinal {
  final val zero: Ord = new Ord(Vector().par)
  final val one:Ord  = Ord(Vector(zero).par)
  final val omega: Ord = Ord(Vector(one).par)
  final val omegaStr: String = "ω"


  final def apply(i: Int): Ord = Ord(Vector.fill(i.max(0))(zero).par)

  final def omegaExpString(str:String): String = {
    if (1==str.length)
      str match {
        case "0" => "1"
        case "1" => Ordinal.omegaStr
        case _ => omegaStr + "^" + str
    } else omegaStr + "^(" + str + ")"
  }

  final def ordVecRemoveInitialSmaller(ords: ParVector[Ord]): ParVector[Ord] = {
    if (ords.isEmpty) ords else ords.drop(ords.indexOf(ords.max))
  }

  final def omegaStack(n: BigInt): Ord = {
    @tailrec
    def go(i: BigInt, alpha: Ord): Ord = {
      if (i < 1) alpha
      else go(i - 1, Ord(Vector(alpha).par))
    }
    go(n, one)
  }

  final def ordVecIsSorted(ords: ParVector[Ord]): Boolean = ords.sameElements(ords.toVector.sorted)

  final def ordVecSortedSubVec(ords: ParVector[Ord]): ParVector[Ord] = {
    @tailrec
    def go(nVec: ParVector[Ord],aVec: ParVector[Ord]): ParVector[Ord] = {
      if (aVec.isEmpty) nVec else go(nVec :+ aVec.head,ordVecRemoveInitialSmaller(aVec.tail))
    }
    go(Vector().par,ordVecRemoveInitialSmaller(ords))
  }

}



object ordinalApp extends App {

  println("****************************")
  logg("Hellenic letters")("α β γ ω")
  println("****************************")
  println("********* Ordinals *********")
  println("****************************")
  println("zero                  = " + zero)
  println("Ordinal(1)            = " + Ordinal(1))
  println("Ordinal(42)           = " + Ordinal(42))
  println("Ordinal(42).isZero    = " + Ordinal(42).isZero)
  println("Ordinal(42).isFinite  = " + Ordinal(42).isFinite)
  println("Ordinal(-42)          = " + Ordinal(-42))
  println("omega                 = " + omega)
  println("omega.isZero          = " + omega.isZero)
  println("omega.isFinite        = " + omega.isFinite)
  println("omega^(42)            = " + Ord(Vector(Ordinal(42)).par))
  println("omega^(42).isZero     = " + Ord(Vector(Ordinal(42)).par).isZero)
  println("omega^(42).isFinite   = " + Ord(Vector(Ordinal(42)).par).isFinite)
  println("****************************")
  println("Ordinal(1).toBigInt   = " + Ordinal(1).toBigInt)
  println("Ordinal(42).toBigInt  = " + Ordinal(42).toBigInt)
  println("omega.toBigInt        = " + omega.toBigInt)
  println("****************************")
  logg("omegaStack(-42)")(omegaStack(-42))
  logg("omegaStack(0)")(omegaStack(0))
  logg("omegaStack(1)")(omegaStack(1))
  logg("omegaStack(2)")(omegaStack(2))
  logg("omegaStack(3)")(omegaStack(3))
  println("****************************")
  val alpha: Ord = Ord(Vector(one,Ordinal(42),zero,one).par)
  val beta: Ord = Ord(Vector(zero,one,omegaStack(2),Ord(Vector(omegaStack(2),omegaStack(3)).par),omegaStack(2),omegaStack(1)).par)
  val gamma: Ord = Ord(Vector(zero,one,omega).par)
  logg("α")(alpha)
  logg("β")(beta)
  logg("γ")(gamma)
  println("****************************")
  logg("α.compare(α)")(alpha.compare(alpha))
  logg("α.compare(β)")(alpha.compare(beta))
  logg("α.compare(γ)")(alpha.compare(gamma))
  logg("β.compare(γ)")(beta.compare(gamma))
  println("****************************")
  logg("α.normalise")(alpha.normalise)
  logg("β.normalise")(beta.normalise)
  logg("γ.normalise")(gamma.normalise)
  println("****************************")
  logg("1.add(ω)")(one.add(omega))
  logg("ω.add(1)")(omega.add(one))
  logg("α.add(β)")(alpha.add(beta))
  logg("β.add(α)")(beta.add(alpha))
  println("****************************")
  println("***** Work in Progress *****")
  println("****************************")
}

/// Local Variables:
/// mode: scala
/// coding: utf-8
/// End:
