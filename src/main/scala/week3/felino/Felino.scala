package week3.felino

trait Felino{
  def color:String
  def sonido:String
}

case class Gato(color:String, sonido:String, comida:String) extends Felino {}
case class Leon(color:String, sonido:String, melena:Double) extends Felino {}
case class Tigre(color:String, sonido:String) extends Felino {}
case class Jaguar(color:String, sonido:String) extends Felino {}

object Main extends App {
  val leon = Leon("amarillo", "grr" , 31.4)
  val tigre = Tigre("naranja", "arr")
  val lista:List[Felino] = List(leon, tigre)
  println(lista)
}