package Inmutable

import org.scalatest.FunSuite
import co.Inmutable.{Const, List, Nil}

class ListHighOrderFunctionTesting extends FunSuite{
  test("funcion sumarUno") {
    val lst = List(1, 2, 3)
    assert(List.sumarUnoFR(lst) === List(2, 3, 4))
  }

  /*
  Al pasar Nil y const como parametros del foldRight se vuelve a formar la lista original de forma recursiva
  * */
  test("funcion fold right con parametros Nil y Const"){
    val lst = List(9L,6L,7L)
    val fun =List.foldRight(lst,Nil:List[Long])(Const(_,_))
    assert(fun === lst)
  }

  test("funcion lenght utilizando la funcion foldRight"){
    val lst = List(1,2,3,4,5,6,7,8)
    assert(List.lenght(lst) === List.lenghtFR(lst))
  }

  test("funcion and utilizando la funcion foldRight con valor false"){
    val lst = List(true,false,true,true,true)
    assert(List.and(lst) === List.andFR(lst))
  }

  test("funcion and utilizando la funcion foldRight con valor true"){
    val lst = List(true,true,true,true,true)
    assert(List.and(lst) === List.andFR(lst))
  }

  test("funcion takewhile"){
    val lst = List(1,2,3,4,5,6,7,8)
    val p = (x:Int)=>{
      x<5 || x>7
    }
    assert(List.takewhile(lst)(p)===List(1,2,3,4))
  }

  test("funcion filter usando folRight"){
    val lst = List(1,2,3,4,5,6,7,8)
    val p = (x:Int)=>{
      x<5 || x>7
    }
    assert(List.filter(lst)(p)===List(1,2,3,4,8))
  }

  test("funcion lenght utilizando la funcion foldLeft"){
    val lst = List(1,2,3,4,5,6,7,8)
    assert(List.lenght(lst) === List.lengthFL(lst))
  }

  test("funcion and utilizando la funcion foldLeft con valor false"){
    val lst = List(true,false,true,true,true)
    assert(List.and(lst) === List.andFL(lst))
  }

  test("funcion and utilizando la funcion foldLeft con valor true"){
    val lst = List(true,true,true,true,true)
    assert(List.and(lst) === List.andFL(lst))
  }

  test("funcion flip"){
    val lst = List(true,true,false)
    assert(List.flip(lst) === List(false,true,true))
  }


  test("funcion takewhile utilizando foldLeft"){
    val lst = List(1,2,3,4,5,6,7,8)
    val p = (x:Int)=>{
      x<5 || x>7
    }
    assert(List.takewhile(lst)(p)===List.takewhileFL(lst)(p))
  }

  test("funcion filter usando folLeft"){
    val lst = List(1,2,3,4,5,6,7,8)
    val p = (x:Int)=>{
      x<5 || x>7
    }
    assert(List.filter(lst)(p)===List.filterFL(lst)(p))
  }

}
