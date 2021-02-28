package Funciones

import org.scalatest.FunSuite

class FunctionTesting extends FunSuite {
  test("Calcular salario") {
    println("calculando el salario basico")
    assert(Funciones.calSalario(3000,1000) === 2000)
  }

  test("Calcular salario bono") {
    println("calculando el salario con bono estatico de 1%")
    assert(Funciones.calSalarioBono(1000,300) === 710)
  }

  test("Calcular salario bono clausura") {
    println("calculando el salario con bono definiendo el bono por fuera de la funcion"
    ,"al no ser una variable global, no la encontrara por ende el bono valdra 0")
    assert(Funciones.calSalarioBonoClausura(1000,300) === 700)
  }

  test("Generador de calculadores de salario") {
    println("llamaremos a un generador de salario del 1% y comprobaremos que funcione correctamente")
    val function = Funciones.genCalSalarioBono(1)
    val a = function(1000,300)
    assert(a === 710)
  }

  test("Funcion factorial") {
    println("probando la funcion factorial")
    assert(Funciones.factorial(10) === 3628800)
  }

  test("Funcion de fibonacci") {
    println("probando la funcion de fibonacci")
    assert(Funciones.fibonacci(5) === 5)
  }

}
