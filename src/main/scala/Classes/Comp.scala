package Classes

object Comp {
  def cuadrado(value:Float):Float = {
    value*value
  }
  def cube(value:Float):Float = {
    value*cuadrado(value)
  }
}
