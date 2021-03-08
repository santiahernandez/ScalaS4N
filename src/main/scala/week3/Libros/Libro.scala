package week3.Libros

class Libro (val titulo:String, val autor:String, val ref:String){
  def nombre = s" $titulo $autor $ref"
}
