package week3.Gente

class Persona (val nombre:String , val apellido:String){
  def getnombre = s"$nombre $apellido"
}

object Persona{
  def apply(nombreCompleto:String = "Juan Cardona"): Persona = {
    val nombre = nombreCompleto.split(" ")(0)
    val apellido = nombreCompleto.split(" ")(1)
    new Persona(nombre,apellido)
  }

}

object Main extends App{
  println(Persona("Santiago Hernandez").nombre)
}