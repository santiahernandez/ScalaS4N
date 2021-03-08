package week3.Gatos

object gatos extends App{
  val Io = new Gato("Io","Fawn","Churrus")
  val Make = new Gato("Make","Red","Milk")
  val Docker = new Gato("Docker","Blue","Cuido")
  println(Io)
  println(Make)
  println(Docker)
  val ventaDeChurrus = new VentaDeChurrus(Io)
  println(s"La comida favorita de ${Io.Nombre} son los churrus? ",ventaDeChurrus.despachar())

}
