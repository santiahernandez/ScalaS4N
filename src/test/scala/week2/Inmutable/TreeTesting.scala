package week2.Inmutable

import org.scalatest.FunSuite
import week2.Inmutable.{Branch, Tree, Leaf}

class TreeTesting extends FunSuite {
  test("funcion size of a tree"){
    val mytree =  Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))
    assert(Tree.size(mytree)===5)
  }

  test("funcion depth of a tree"){
    val mytree =  Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))
    assert(Tree.depth(mytree)===3)
  }

  test("funcion sumar of a tree"){
    val mytree =  Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))
    assert(Tree.sumar(mytree)===6)
  }





}
