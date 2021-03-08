package week3.Gatos

class Gato (val Nombre:String, val Color:String, val Comida:String){
  override def toString: String = s"$Nombre es un gato de color $Color y su comida favorita es $Comida"
}
