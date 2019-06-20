package ordinals

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector
import Ordinal._
import util._

import scala.util.Random


sealed trait Ordinal

case class Ord(ords: ParVector[Ord]) extends Ordered[Ord]  {
  final def getExpVec: ParVector[Ord] = ords

  final override def toString: String = {
    // a beautiful string
    if (isFinite) toBigInt.get.toString else {
      val expStrs: ParVector[String] = ords.map(_.toString).map(Ordinal.omegaExpString)
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
        go(normalise.ords,that.normalise.ords)
      }
    }
  }

  final def equals(that: Ord): Boolean = 0 == compare(that)

  final def degree: Ord = getExpVec.max

  final def isZero: Boolean = (this == zero)
  final def isExp: Boolean = (1 == getExpVec.size)
  final def isFinite: Boolean = getExpVec.forall(_.isZero)
  final def isLimit: Boolean = !getExpVec.last.isZero
  final def isSuccessor: Boolean = getExpVec.last.isZero


  final def toInt: Option[Int] = if (isFinite) Some(getExpVec.length) else None
  // toInt returns Some(i) iff this is the finite ordinal i
  final def toBigInt: Option[BigInt] = if (isFinite) Some(getExpVec.length) else None
  // toBigInt returns Some(i) iff this is the finite ordinal i

  final def predecessor: Option[Ord] = if (isSuccessor) Option(Ord(getExpVec.init)) else None
  final def successor: Ord = Ord(getExpVec :+ zero)
  final def exp: Ord = Ord(ParVector(this))

  final def normalise: Ord = {
    if (isFinite) this else {
      Ord(ordVecSortedSubVec(getExpVec).map(_.normalise))
    }
  }
  final def isNormal: Boolean = {
    if (isFinite) true
    else {
      val exps = getExpVec
      ordVecIsSorted(exps) && exps.forall(_.isNormal)
    }
  }

  final def degreePart: Ord = Ord(normalise.getExpVec.filter(_.equals(degree)))
  final def nonDegreePart: Ord = Ord(normalise.getExpVec.filterNot(_.equals(degree)))
  final def finitePart: Ord = Ord(normalise.getExpVec.filter(_.isZero))
  final def limitPart: Ord = Ord(normalise.getExpVec.filterNot(_.isZero))

  final def add(that: Ord): Ord = Ord(getExpVec++that.getExpVec).normalise
  final def add(n: Int): Ord = add(Ordinal(n))

  final def mult(that: Ord): Ord = {
    // α * (β + γ) = α*β + α*γ
    // that = thatLimit + thatFinite
    val thisDegree: Ord = degree
    val thatFinit: Int = that.finitePart.toInt.get
    val thisTimesThatLimit: Ord = Ord(that.limitPart.getExpVec.map(thisDegree.add(_)))
    if (0==thatFinit) thisTimesThatLimit else {
      val thisTimesThatFinite: Ord = Ord(ParVector.fill(degreePart.getExpVec.size * thatFinit)(thisDegree)).add(nonDegreePart)
      thisTimesThatLimit.add(thisTimesThatFinite)
    }
  }

  final def fSeq:Fseq = Fseq(this)
}


case class Fseq(lambda: Ord) {
  final def isLimit: Boolean = lambda.isLimit
  final def getLimit: Option[Ord] = if (isLimit) Option(lambda) else None
  final def getSeq: Option[Int => Ord] = if(isLimit) {
    val expVec: ParVector[Ord] = lambda.getExpVec
    val expVecSize: Int = expVec.size
    val expLast: Ord = expVec.last
    if (1==expVecSize) {
      // this = ω^α
      if(expLast.isSuccessor) Option(n => expLast.predecessor.get.exp.add(n.abs))
      else Option(n => Fseq(expLast).getSeq.get(n.abs).exp)
    }
    else Option(n=>Ord(expVec.init).add(Fseq(expVec.last.exp).getSeq.get(n.abs)))
  }
  else None
}


object Ordinal {
  final val zero: Ord = new Ord(ParVector())
  final val one:Ord  = zero.exp
  final val two:Ord  = Ord(ParVector(zero,zero))
  final val omega: Ord = one.exp
  final val omegaStr: String = "ω"


  final def apply(n: Int): Ord = Ord(ParVector.fill(n.abs)(zero))

  final def generate(N: Int): Ord = {
 ///   logg("Ordinal.generate N")(N)
    /// Generates an ordinal between 0 and omegaStack(N)
    def go(N: Int,k: Int,alphas: ParVector[Ord]): ParVector[Ord] = {
      if (0==N) ParVector.fill(k)(zero) //Ord(alphas))
      else {
//        val mc: Int = scala.math.pow(2,2.max(7-N)).toInt
        val mc: Int = 3.max(32-16*N)
        val nextK: Int = Random.nextInt(mc).abs.max(Random.nextInt(mc).abs)
//        logg("Ordinal.generate.go (N,k,mc,nextK)")((N,k,mc,nextK))
        ParVector.fill(k)(Ord(go(N-1,nextK,alphas)))
      }
    }
    go(N,1,ParVector(zero)).head.normalise
  }

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

  final def omegaStack(n: Int): Ord = {
    @tailrec
    def go(i: Int, alpha: Ord): Ord = if (i < 1) alpha else go(i - 1, alpha.exp)
    go(n, one)
  }

  final def ordVecIsSorted(ords: ParVector[Ord]): Boolean = ords.sameElements(ords.toVector.sorted.reverse.par)

  final def ordVecSortedSubVec(ords: ParVector[Ord]): ParVector[Ord] = {
    @tailrec
    def go(nVec: ParVector[Ord],aVec: ParVector[Ord]): ParVector[Ord] = {
      if (aVec.isEmpty) nVec else go(nVec :+ aVec.head,ordVecRemoveInitialSmaller(aVec.tail))
    }
    go(ParVector(),ordVecRemoveInitialSmaller(ords))
  }

  final def rec[A](z: A)(succOp: A => A)(limOp: (Int => A) =>  A)(alpha: Ord) :A = {
    if (alpha.isZero) z
    else if (alpha.isSuccessor) succOp(rec[A](z)(succOp)(limOp)(alpha.predecessor.get))
    else limOp(n => rec[A](z)(succOp)(limOp)(Fseq(alpha).getSeq.get(n)))
  }

  final def recAddition(alpha: Ord)(beta: Ord):Ord = rec[Ord](alpha)(_.successor)(???)(alpha)

}



object ordinalApp extends App {

  println("****************************")
  logg("Hellenic letters")("α β γ ω")
  println("****************************")
  println("********* Ordinals *********")
  println("****************************")
  logg("zero")(zero)
  logg("Ordinal(1)")(Ordinal(1))
  logg("Ordinal(42)")(Ordinal(42))
  logg("Ordinal(-42)")(Ordinal(-42))
  logg("omega")(omega)
  logg("omega.isZero")(omega.isZero)
  logg("omega.isFinite")(omega.isFinite)
  logg("omega^(42)")(Ord(ParVector(Ordinal(42))))
  println("****************************")
  logg("omegaStack(-42)")(omegaStack(-42))
  logg("omegaStack(0)")(omegaStack(0))
  logg("omegaStack(1)")(omegaStack(1))
  logg("omegaStack(2)")(omegaStack(2))
  logg("omegaStack(3)")(omegaStack(3))
  println("****************************")
  val alpha: Ord = Ord(ParVector(one,one,Ordinal(42),zero,one,zero,zero))
  val beta: Ord = Ord(ParVector(zero,one,omegaStack(2),Ord(ParVector(omegaStack(2),omegaStack(3))),omegaStack(2),omegaStack(1)))
  val gamma: Ord = Ord(ParVector(zero,one,omega))
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
  logg("0.fSeq")(zero.fSeq)
  logg("1.fSeq")(one.fSeq)
  logg("Fseq(ω)(0)")(Fseq(omega).getSeq.get(0))
  logg("Fseq(ω)(42)")(Fseq(omega).getSeq.get(42))
  logg("Fseq(ω)(-42)")(Fseq(omega).getSeq.get(-42))
  logg("Fseq(ω²)(42)")(Fseq(two.exp).getSeq.get(42))
  logg("ω^ω.fSeq(42)")(Fseq(omega.exp).getSeq.get(42))
  logg("omegaStack(3).fSeq(0)")(Fseq(omegaStack(3)).getSeq.get(0))
  logg("omegaStack(3).fSeq(1)")(Fseq(omegaStack(3)).getSeq.get(1))
  logg("omegaStack(3).fSeq(42)")(Fseq(omegaStack(3)).getSeq.get(42))
  println("****************************")
  logg("generate(0)")(generate(0))
  logg("generate(1)")(generate(1))
  logg("generate(1)")(generate(1))
  logg("generate(2)")(generate(2))
  logg("generate(2)")(generate(2))
  logg("generate(3)")(generate(3))
  logg("generate(3)")(generate(3))
  logg("generate(4)")(generate(4))
  logg("generate(4)")(generate(4))
  logg("generate(8)")(generate(8))
  logg("generate(10)")(generate(10))
  logg("generate(12)")(generate(12))
  println("****************************")
  println("***** Work in Progress *****")
  println("****************************")
  val testOrd: Ord = Ord(ParVector(Ordinal(14).exp.mult(two),Ordinal(8).exp.mult(two)))
  val testString = "ω^(ω^(14)+ω^(14))+ω^(ω^8+ω^8)"
  logg("testOrd")(testOrd)
  logg("testOrd == testString")(testOrd.toString == testString)
  logg("testOrd.normalise")(testOrd.normalise)
  logg("testOrd.normalise.isNormal")(testOrd.normalise.isNormal)
}
/// Local Variables:
/// mode: scala
/// coding: utf-8
/// End:
