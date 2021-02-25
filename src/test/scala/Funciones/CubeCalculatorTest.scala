package week1
package Funciones

import org.scalatest.FunSuite

class CubeCalculatorTest extends FunSuite {
  test("CubeCalculator.cube") {
    println("calculando el cubo de", 3 , " = ", CubeCalculator.cube(3))
    assert(CubeCalculator.cube(3) === 27)
  }
}