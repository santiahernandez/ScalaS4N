package week1
package Inmutable

import org.scalatest.FunSuite
import co.Inmutable.{Const, List, Nil}

class ListTesting extends FunSuite {

  test("Ejercicio1 test") {

    val x = List(4,5,6,7,8) match {
      case Const(x , Const(5, Const(7,_)))            => x
      case Nil                                        => 1
      case Const(x,Const(y , Const(6,Const(7,_))))    => x+y
      case Const(h,t)                                 => h+List.sum(t)
      case _                                          => 777
    }
    assert(x === 9)
  }

  test("funcion tail"){
    val lst1 = List(1,2,3,4)
    val lst2 = List(2,3,4)
    assert(List.tail(lst1) === lst2)
  }

  test("funcion head"){
    val lst1 = List(1,2,3,4)
    val head = 1
    assert(List.head(lst1) === head)
  }

  test("funcion and dando resultado true"){
    val lst1 = List(true,true,true,true,true)
    assert(List.and(lst1) === true)
  }

  test("funcion and dando resultado false"){
    val lst1 = List(true,true,true,false,true)
    assert(List.and(lst1) === false)
  }

  test("funcion or dando resultado false"){
    val lst1 = List(false,false,false,false,false)
    assert(List.or(lst1) === false)
  }

  test("funcion or dando resultado true"){
    val lst1 = List(false,false,false,true,false)
    assert(List.or(lst1) === true)
  }

  test("funcion maximo"){
    val lst = List(4,5,1,8,3)
    assert(List.max(lst) === 8)
  }

  test("funcion minimo"){
    val lst = List(4L,5L,1L,8L,3L)
    assert(List.min(lst) === 1)
  }

  test("funcion minMax"){
    val lst = List(4D,5,7,2,9,3)
    assert(List.minMax(lst) === (2,9))
  }

}
