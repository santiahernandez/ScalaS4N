package week1
package Inmutable

import org.scalatest.FunSuite
import co.Inmutable.List

class ListConstTesting extends FunSuite {
  test("funcion addEnd") {
    val lst1 = List(1, 2, 3, 4)
    val lst2 = List(1, 2, 3, 4, 5)
    assert(List.addEnd(lst1, 5) === lst2)
  }

  test("funcion append") {
    val lst1 = List(1, 2, 3)
    val lst2 = List(4, 5)
    val lst3 = List(1, 2, 3, 4, 5)
    assert(List.append(lst1, lst2) === lst3)
  }

  test("funcion drop") {
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(3, 4, 5)
    assert(List.drop(2, lst1) === lst2)
  }

  test("funcion split") {
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(3, 4, 5)
    val lst3 = List(1,2)
    assert(List.split(2, lst1) === (lst3,lst2))
  }

  test("funcion init"){
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(1,2,3,4)
    assert(List.init(lst1)===lst2)
  }

  test("Funcion zip"){
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(true,false,true)
    val esp = List((1,true),(2,false),(3,true))
    assert(List.zip(lst1,lst2)===esp)
  }

}
