package Classes

object pueba {
  def x: Int = {
    println("x")
    1
  }

  def y: Int = {
    println("y")
    2
  }

  def z: String = {
    println("z")
    x
    x+"c"
  }


}
