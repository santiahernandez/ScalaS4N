package week3.Figuras

class Circulo (radio:Double) extends Forma{
  override def area: Double = radio*radio*math.Pi
}
