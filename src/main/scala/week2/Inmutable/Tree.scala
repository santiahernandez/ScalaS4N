package week2.Inmutable

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A],right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1+size(left)+size(right)
  }

  def depth[A](tree: Tree[A]):Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1+math.max(depth(left),depth(right))
    }
  }

  def sumar(tree: Tree[Int]):Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => sumar(left) + sumar(right)
    }
  }

}
