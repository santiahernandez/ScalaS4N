package week3.Gatos

class VentaDeChurrus (val gato: Gato){
  def despachar():Boolean = gato.Comida match {
    case "Churrus" => true
    case _ => false
  }
}
