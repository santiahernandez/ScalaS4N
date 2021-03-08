package week3.Automovilismo

object automovilismo extends App{
  val JuanPablo = new Conductor("Juan Pablo","Montoya",63523,61313)
  val Maclaren = new Escuderia("Maclaren",JuanPablo)
  println(Maclaren.conductor.getCarrerasNoTerminadas)
}
