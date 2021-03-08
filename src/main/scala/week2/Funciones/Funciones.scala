package week2.Funciones

import math.Pi
import scala.annotation.tailrec

object Funciones extends App{

  def areaTrianguloRectangulo (a:Int , b:Int):Double = {
    (a + b) /2
  }

  val areaDeUnCirculo = new (Int => Double) {
    def apply(radio:Int):Double ={
      Pi * radio * radio
    }
  }

  def calSalario  (devengado:Double , deducciones:Double):Double =  {
    devengado - deducciones
  }
  def calSalarioBono  (devengado:Double , deducciones:Double):Double = {
    devengado*1.01-deducciones
  }

  def compSalario(funcion:(Double , Double) => Double,devengado:Double, deducciones:Double) = {
    funcion(devengado,deducciones)
  }

  def genCalSalarioBono (bono:Double):(Double,Double) => Double = {
    (devengado:Double , deducciones:Double) => {
      val aux = 1+ ( bono / 100 )
      (devengado * aux)-deducciones
    }
  }

  //la variable bono solo sera reconocida internamente en el objeto funciones
  val bono = 1.0
  def calSalarioBonoClausura (devengado:Double , deducciones:Double):Double = {
    devengado*(1+(bono/100))-deducciones
  }

  def factorial (n:Int ): Long ={
    @tailrec
    def factorialEnCola (n:Int , cont:Long): Long = {
      n match {
        case 1 => cont
        case _ => factorialEnCola(n-1,cont*n)
      }
    }
    factorialEnCola(n,1)
  }

  def fibonacci (n:Int):Int = {
    if (n==0) 0
    else if (n==1) n
    else fibonacci(n-1)+fibonacci(n-2)
  }

}
