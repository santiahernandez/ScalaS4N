package week3.TraitFiguras

sealed trait Forma {
  def tamano:Int
  def area:Double
  def perimetro:Double


}



trait Rectangular extends Forma {
  val base:Double
  val altura:Double
  override def area: Double = base*altura

  override def perimetro: Double = 2*(base+altura)

  override def tamano: Int = 4
}

case class Cuadrado(lado:Double) extends Rectangular {
  override val base: Double = lado
  override val altura: Double = lado
}

case class Rectangulo(base:Double,altura:Double) extends Rectangular {
}

case class Circulo(radio:Double) extends Forma {
  override def area: Double = math.Pi*math.pow(radio,2)

  override def perimetro: Double = math.Pi*2*radio

  override def tamano: Int = 0
}

object MainFiguras extends App{
  val rec = Rectangulo(2,3)
  val cuad = Cuadrado(4)
  println("Rectangulo: ",rec.perimetro ," Cuadrado: " ,cuad.perimetro)

}