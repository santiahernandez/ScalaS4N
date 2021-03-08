package week3.Contadores

class Contador (val contador: Int, val sumador: Sumador){
  def decr():Contador = new Contador(contador-1,sumador)
  def incr():Contador = new Contador(contador+1,sumador)

  def decr (value:Int):Contador = new Contador(contador-value,sumador)
  def incr (value:Int):Contador = new Contador(contador+value,sumador)

  def decrwithsumador (value:Int):Contador = new Contador(contador-sumador.adicionar(value),sumador)
  def incrwithsumador (value:Int):Contador = new Contador(contador+sumador.adicionar(value),sumador)

}
