package week4.Exeptions

import org.scalatest.FunSuite

class ExeptHandlingTesting extends FunSuite{
  test("fail funcion have to not be working, always return 43") {
    assert(ExeptHandling.failingFun(10) === 43)
  }

  test("media of empty list") {
    assert(ExeptHandling.media(List()) === None)
  }

  test("media of some list") {
    assert(ExeptHandling.media(List(1.2,3.2,4.3,4.5)) === Some(3.3))
  }

}
