package week3.Automovilismo

class Conductor (val nombre:String, val apellido:String, val totalCarreras:Int, val carrerasTerminadas:Int){
  def getCarrerasNoTerminadas:Int=totalCarreras-carrerasTerminadas
}
