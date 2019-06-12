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
    if (isFinite) toBigInt.get.toString else {
      val expStrs: ParVector[String] = getExpList.map(_.toString).map(Ordinal.omegaExpString)
      val lastInfinite: Int = expStrs.lastIndexWhere(_ != "1")
      // this = α + finiteEnd
      val finiteEnd: Int = expStrs.size-lastInfinite-1
      (expStrs.take(1+lastInfinite) ++ Vector.fill[String](finiteEnd.signum)(finiteEnd.toString)).mkString("+")
      //      expStrs.mkString("+")
    }
  }

  final def compare(that: Ord) = {
    if (isFinite)
    // if both ordinals finite use bigInt comparison
    // else finite is smaller than infinite
      if (that.isFinite) toBigInt.get compare that.toBigInt.get else -1
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
        go(normalise.getExpList,that.normalise.getExpList)
      }
    }
  }

  final def equals(that: Ord): Boolean = 0 == compare(that)

  final def degree: Ord = getExpList.max

  final def isZero: Boolean = (this == zero)

  final def isFinite: Boolean = getExpList.forall(_.isZero)

  final def toInt: Option[Int] = if (isFinite) Some(getExpList.length) else None
  // toInt returns Some(i) iff this is the finite ordinal i

  final def toBigInt: Option[BigInt] = if (isFinite) Some(getExpList.length) else None
  // toBigInt returns Some(i) iff this is the finite ordinal i

  final def normalise: Ord = {
    if (isFinite) this else {
      Ord(ordVecSortedSubVec(getExpList).map(_.normalise))
    }
  }

  final def isNormal: Boolean = {
    if (isFinite) true
    else {
      val exps = getExpList
      ordVecIsSorted(exps) && exps.forall(_.isNormal)
    }
  }

  final def degreePart: Ord = Ord(normalise.getExpList.filter(_.equals(degree)))
  final def nonDegreePart: Ord = Ord(normalise.getExpList.filterNot(_.equals(degree)))
  final def finitePart: Ord = Ord(normalise.getExpList.filter(_.isZero))
  final def limitPart: Ord = Ord(normalise.getExpList.filterNot(_.isZero))

  final def add(that: Ord): Ord = Ord(getExpList++that.getExpList).normalise
  final def add(n: Int): Ord = add(Ordinal(n))

  final def mult(that: Ord): Ord = {
    // α * (β + γ) = α*β + α*γ
    // that = thatLimit + thatFinite
    val thisDegree: Ord = degree
    val thatFinit: Int = that.finitePart.toInt.get
    val thisTimesThatLimit: Ord = Ord(that.limitPart.getExpList.map(thisDegree.add(_)))
    if (0==thatFinit) thisTimesThatLimit else {
      val thisTimesThatFinite: Ord = Ord(Vector.fill(degreePart.getExpList.size * thatFinit)(thisDegree).par).add(nonDegreePart)
      thisTimesThatLimit.add(thisTimesThatFinite)
    }
  }
}


object Ordinal {
  final val zero: Ord = new Ord(Vector().par)
  final val one:Ord  = Ord(Vector(zero).par)
  final val two:Ord  = Ord(Vector(zero,zero).par)
  final val omega: Ord = Ord(Vector(one).par)
  final val omegaStr: String = "ω"


  final def apply(i: Int): Ord = Ord(Vector.fill(i.max(0))(zero).par)

  final def omegaExpString(str:String): String = {
    if (1==str.length)
      str match {
        case "0" => "1"
        case "1" => Ordinal.omegaStr
        case "2" => Ordinal.omegaStr + "²"
        case "3" => Ordinal.omegaStr + "³"
        case "4" => Ordinal.omegaStr + "⁴"
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
  println("Ordinal(-42)          = " + Ordinal(-42))
  println("omega                 = " + omega)
  println("omega.isZero          = " + omega.isZero)
  println("omega.isFinite        = " + omega.isFinite)
  println("omega^(42)            = " + Ord(Vector(Ordinal(42)).par))
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
  val alpha: Ord = Ord(Vector(one,one,Ordinal(42),zero,one,zero,zero).par)
  val beta: Ord = Ord(Vector(zero,one,omegaStack(2),Ord(Vector(omegaStack(2),omegaStack(3)).par),omegaStack(2),omegaStack(1)).par)
  val gamma: Ord = Ord(Vector(zero,one,omega).par)
  /*
  α	= ω+ω+ω^(42)+1+ω+2
  β	= 1+ω+ω^(ω^ω)+ω^(ω^(ω^ω)+ω^(ω^(ω^ω)))+ω^(ω^ω)+ω^ω
  γ	= 1+ω+ω^ω
   */
  logg("α")(alpha)
  logg("β")(beta)
  logg("γ")(gamma)
  println("****************************")
  logg("α.compare(α)")(alpha.compare(alpha))
  logg("α.compare(β)")(alpha.compare(beta))
  logg("α.compare(γ)")(alpha.compare(gamma))
  logg("β.compare(γ)")(beta.compare(gamma))
  println("****************************")
  logg("Ordinal(42).degree")(Ordinal(42).degree)
  logg("α.degree")(alpha.degree)
  logg("β.degree")(beta.degree)
  logg("γ.degree")(gamma.degree)
  println("****************************")
  logg("α.normalise")(alpha.normalise)
  logg("β.normalise")(beta.normalise)
  logg("γ.normalise")(gamma.normalise)
  println("****************************")
  logg("Ordinal(42).add(Ordinal(23))")(Ordinal(42).add(Ordinal(23)))
  logg("1.add(ω)")(one.add(omega))
  logg("ω.add(1)")(omega.add(one))
  logg("α.add(β)")(alpha.add(beta))
  logg("β.add(α)")(beta.add(alpha))
  println("****************************")
  logg("Ordinal(42).mult(Ordinal(23))")(Ordinal(42).mult(Ordinal(23)))
  logg("(ω²+ω+1).mult(3)")(Ord(ParVector(two,one,zero)).mult(Ordinal(3)))
  logg("1.mult(ω)")(one.mult(omega))
  logg("ω.mult(1)")(omega.mult(one))
  logg("β.mult(0)")(beta.mult(zero))
  logg("β.mult(1)")(beta.mult(one))
  logg("β.mult(2)")(beta.mult(two))
  logg("α.mult(β)")(alpha.mult(beta))
  logg("β.mult(α)")(beta.mult(alpha))
  println("****************************")
  println("***** Work in Progress *****")
  println("****************************")
}

/// Local Variables:
/// mode: scala
/// coding: utf-8
/// End:
