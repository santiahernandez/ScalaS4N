package week1
package Inmutable
import org.scalatest.FunSuite
import co.Inmutable.{Cero, Nat, Suc}

class NatTesting extends FunSuite {
  test("haciendo una prueba"){
    val uno =  Suc(Cero)
    val dos = Suc(Suc(Cero))
    assert(Suc(uno) === dos)
  }

  test("From nat to int"){
    val cuatro = Suc(Suc(Suc(Suc(Cero))))
    assert(Nat.fromNatToInt(cuatro) === 4)
  }

  test("From Int to Nat"){
    val cuatro = Suc(Suc(Suc(Suc(Cero))))
    assert(Nat.fromIntToNat(4) === cuatro)
  }
}
