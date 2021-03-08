package week3.TraitColores

abstract class Color(){
  def RED:Int
  def GREEN:Int
  def BLUE:Int
  def getColor:String = s"your color is R:$RED G:$GREEN B:$BLUE"
}


object Color {
  def apply(r:Int,g:Int,b:Int):Color = new Color {
    override def RED: Int = r

    override def GREEN: Int = g

    override def BLUE: Int = b
  }
}

case object ROSA extends Color {
  override def RED: Int = 255
  override def GREEN: Int = 0
  override def BLUE: Int = 102
}


case object AMARILLO extends Color {
  override def RED: Int = 255
  override def GREEN: Int = 255
  override def BLUE: Int = 0
}

case object ROJO extends Color {
  override def RED: Int = 255
  override def GREEN: Int = 0
  override def BLUE: Int = 0
}

object colores extends App{
  val mycolor = Color(102,255,102)
  println(mycolor.getColor)
  println(ROJO.getColor)
}