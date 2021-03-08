package week3.CompanionClasess

class Counter (val value:Int = 1 , val step:Int = 1) {
  def doStep():Counter = copy(value+step)
  def doStep(step:Int):Counter= copy(value + step,step)
  override def toString:String = s"VALUE: $value step: $step"
  def copy(value:Int = this.value , step:Int = this.step) = new Counter(value,step)
}

object Counter{
  def apply(value:Int = 1, step:Int = 1) = new Counter(value,step)
}

object main extends App {
  val c: Counter = Counter()
  val c2:Counter = Counter(2,2)

  println(c.doStep(1).doStep(2).doStep(3))
  println(c2.doStep(1).doStep(2).doStep(3))
}

