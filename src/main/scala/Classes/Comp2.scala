package Classes

object Comp2 {
  def cuadrado(value:Long):Long = {
    value*value
  }
  def cube(value:Long):Long = {
    value*cuadrado(value)
  }
}

