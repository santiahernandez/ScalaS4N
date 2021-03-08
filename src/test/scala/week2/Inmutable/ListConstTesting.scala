package week2.Inmutable

import org.scalatest.FunSuite
import week2.Inmutable.List

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

  test("Funcion unzip"){
    val lst3 = List(1, 2, 3)
    val lst2 = List(true,false,true)
    val lst1 = List((1,true),(2,false),(3,true))
    assert(List.unzip(lst1)===(lst3,lst2))
  }

  test("Funcion reverse"){
    val lst1 = List(1, 2, 3)
    val lst2 = List(3,2,1)
    assert(List.reverse(lst1)===lst2)
  }

  test("Funcion interprese"){
    val lst1 = List(1, 2, 3)
    val lst2 = List(1,0,2,0,3,0)
    assert(List.interprese(0,lst1)===lst2)
  }

  test ("Funcion concat"){
    val lst1 = List(List(1,2,3),List(4,5,6),List(7,8,9))
    val lst2 = List(1,2,3,4,5,6,7,8,9)
    assert(List.concat(lst1)===lst2)
  }

}
