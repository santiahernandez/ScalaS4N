package week4.Exeptions

object ExeptHandling {
  def failingFun(i: Int):Int = {
    try {
      val x = 42 + i
      val y : Int = throw new Exception("fail!")
      x + y
    }
    catch {
      case e: Exception => 43
    }
  }


  def media(list:List[Double]):Option[Double] = {
    if(list.isEmpty) None else Some(list.sum/list.length)
  }
}
