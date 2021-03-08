package week3.Contadores

object Contar extends App {
  val mySumador = new Sumador(0)
  val mycounter = new Contador(563423,mySumador)
  println(mycounter.decr(10).decr().incr(2).decr().incr(4623).decrwithsumador(5344).incrwithsumador(6453).contador)
}
