package week1
package co.Inmutable

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

}