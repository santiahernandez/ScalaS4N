package week2.Inmutable

import scala.annotation.tailrec

sealed trait Nat
case object Cero extends Nat
case class Suc( n: Nat) extends Nat

object Nat {

  def fromNatToInt (n:Nat):Int =n match {
    case Cero => 0
    case Suc(h) => 1+fromNatToInt(h)
  }

  def fromIntToNat (i:Int):Nat = i match {
    case 0 => Cero
    case h => Suc(fromIntToNat(h-1))
  }

  def addNat(nat1:Nat, nat2: Nat):Nat ={
    @tailrec
    def addNatIntern(nat1:Nat,nat2:Nat,nataux:Nat):Nat=(nat1,nat2) match {
      case (Cero,Cero) => nataux
      case (Cero,_) => addNatIntern(nat2,Cero,nataux)
      case (Suc(n1),_) => addNatIntern(n1,nat2, Suc(nataux))
    }
    addNatIntern(nat1,nat2,Cero)
  }

  def prodNat(nat1:Nat, nat2: Nat):Nat ={
    @tailrec
    def prodNatIntern(nat1:Nat,nat2:Nat,nataux:Nat):Nat=(nat1,nat2) match {
      case (Cero,_) => Cero
      case (_,Cero) => Cero
      case (Suc(Cero),n) => addNat(n,nataux)
      case (Suc(n1),_) => prodNatIntern(n1,nat2,addNat(nataux,nat2))

    }
    prodNatIntern(nat1,nat2,Cero)
  }
}