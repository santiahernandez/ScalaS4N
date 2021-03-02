package co.Inmutable

import scala.annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

object List {
  def lenght[A](lst:List[A]):Int =  lst match{
    case Nil => 0
    case Const(_,t) => 1 + lenght(t)
  }

  def tail[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case Const(_,t) => t
  }

  def head[A](lst:List[A]):A = lst match {
    case Const(h,_) => h
  }


  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Const(h,t) => h + sum(t)
  }

  @tailrec
  def and(lst:List[Boolean]):Boolean = lst match {
    case Nil => true
    case Const(true,t) => and(t)
    case Const(false,_) => false
  }

  @tailrec
  def or(lst:List[Boolean]):Boolean = lst match {
    case Const(true,Nil) => true
    case Const(false,Nil) => false
    case Const(false,t) => or(t)
    case Const(true, _) => true
  }

  def max (ints:List[Int]):Int = {
    def maxim (a:Int , b:Int):Int = {
      if (a>=b) a
      else b
    }
    ints match {
      case Const(h,Nil) => h
      case Const(h,t) => maxim(h,max(t))
    }
  }

  def min (ints:List[Long]):Long = {
    def minim (a:Long , b:Long):Long = {
      if (a>=b) b
      else a
    }
    ints match {
    case Const(h,Nil) => h
    case Const(h,t) => minim(h,min(t))
    }
  }

  def minMax(lst:List[Double]):(Double,Double) = {

    def maximDouble (a:Double , b:Double):Double = {
      if (a>=b) a
      else b
    }

    def minimDouble (a:Double , b:Double):Double = {
      if (a>=b) b
      else a
    }

    @tailrec
    def maxMintemp (doub:List[Double],minimo:Double,maximo:Double):(Double,Double)= doub match {
      case Nil => (0,0)
      case Const(h,Nil) => (minimDouble(maximo,h),maximDouble(minimo,h))
      case Const(h,t) => maxMintemp(t,maximDouble(h,minimo),minimDouble(h,maximo))
    }

    maxMintemp(lst,Double.MinValue,Double.MaxValue)
  }

  def const[A](h:A, t:List[A]):List[A] = Const(h, t)

  def addEnd[A](lst:List[A], elem:A):List[A] = lst match{
    case Nil          => Const(elem, Nil)
    case Const(h, t)  => Const(h, addEnd(t, elem))
  }

  def append[A](lst1:List[A], lst2:List[A]):List[A] = (lst1, lst2) match{
    case (Nil, Nil)   => Nil
    case (lst1, Nil)  => lst1
    case (Nil, lst2)  => lst2
    case (Const(h, t), lst2) => Const(h, append(t, lst2))
  }

  @tailrec
  def drop[A](n:Int, lst:List[A]): List[A] = (n, lst) match {
    case (0, lst) => lst
    case (_, Nil) => Nil
    case (n, Const(_, t)) => drop(n-1, t)
  }

  def split[A](n:Int, lst:List[A]) : (List[A], List[A]) = {
    @tailrec
    def splitintern(n:Int, lst:List[A] , lstaux:List[A]):(List[A], List[A]) = (n,lst) match {
    case  (0,lst) => (lstaux,lst)
    case (_,Nil) => (Nil,Nil)
    case (n,Const(h,t)) => splitintern(n-1,t,addEnd(lstaux,h))
    }
    splitintern(n,lst,Nil)
  }

  def take[A](n:Int, lst:List[A]):List[A] ={
    @tailrec
    def takeinside(n:Int, lst:List[A] , lstaux:List[A]): List[A] = (n, lst) match {
      case (0, lst) => lst
      case (_, Nil) => lstaux
      case (n, Const(h, t)) => takeinside(n-1, t,addEnd(lstaux,h))
    }
    takeinside(n,lst,Nil)
  }

  def init [A](lst:List[A]):List[A] ={
    @tailrec
    def initinside(lst:List[A] , lstaux:List[A]):List[A] = lst match {
      case Nil => Nil
      case Const(_,Nil) => lstaux
      case Const(h,t) => initinside(t, addEnd(lstaux,h))
    }
    initinside(lst,Nil)
  }

  def zip[A,B](lst1:List[A],lst2:List[B]):List[(A,B)]= {
    @tailrec
    def zipin(lst1:List[A], lst2:List[B], lstaux:List[(A,B)]):List[(A,B)]=  (lst1,lst2) match {
      case (Nil,_) => lstaux
      case (_,Nil) => lstaux
      case (Const(h1,t1) , Const(h2,t2)) => zipin(t1,t2,addEnd(lstaux,(h1,h2)))
    }
    zipin(lst1,lst2,Nil)
  }

  def unzip[A,B](lst:List[(A,B)]):(List[A] , List[B])= {
    @tailrec
    def unzipin(lst:List[(A,B)], lstaux1:List[A],lstaux2:List[B]):(List[A] , List[B])=  lst match {
      case Nil => (lstaux1,lstaux2)
      case Const((h1,h2),t) => unzipin(t,List.addEnd(lstaux1,h1),List.addEnd(lstaux2,h2))
    }
    unzipin(lst,Nil,Nil)
  }

  def reverse[A](lst:List[A]):List[A]={
    @tailrec
    def reverseintern(lst:List[A],lstaux:List[A]):List[A]=lst match {
      case Nil => lstaux
      case Const(h,t) => reverseintern(t,Const(h,lstaux))
    }
    reverseintern(lst,Nil)
  }

  def interprese[A](n:A, lst:List[A]):List[A]={
    @tailrec
    def interpreseIntern(dat:A,lst:List[A],lstaux:List[A]):List[A]= (dat,lst) match {
      case (_,Nil) => lstaux
      case (dat,Const(h,t)) => interpreseIntern(dat,t,List.addEnd(List.addEnd(lstaux,h),dat))
    }
    interpreseIntern(n,lst,Nil)
  }

  def concat[A](lst: List[List[A]]):List[A]={
    @tailrec
    def concatIntern(lst:List[List[A]],lstaux:List[A]):List[A]= lst match {
      case Nil => lstaux
      case Const(h,t) => concatIntern(t,List.append(lstaux,h))
    }
    concatIntern(lst,Nil)
  }

  /*
  Funciones de orden superior
  * */

  def foldRight[A,B](as:List[A],z:B)(f:(A,B)=>B):B = as match {
    case Nil => z
    case Const(h,t) => f(h,foldRight(t,z)(f))
  }

  def sumFR(lst:List[Int]):Int = foldRight(lst,0)(_+_)

  def mulFR(lst:List[Int]): Int = foldRight(lst,1)(_*_)

  def lenghtFR[A](lst:List[A]):Int = foldRight(lst,0)((_,y)=>y+1)

  @tailrec
  def foldLeft[A,B](lst:List[A], z:B)(f:(B,A)=>B):B = lst match {
    case Const(h, t) => foldLeft(t,f(z,h))(f)
    case Nil => z
  }

  def sumFL(lst:List[Int]):Int = foldLeft(lst,0)(_+_)

  def mulFL(lst:List[Int]): Int = foldLeft(lst,1)(_*_)

  def sumarUnoFL(lst:List[Int]): List[Int] = foldRight(lst,Nil:List[Int])((h,t)=>Const(h+1,t))

  def map[A,B](lst:List[A])(f:A=>B):List[B] = foldRight(lst,Nil:List[B])((x,y) => Const(f(x),y))

  def andFR(lst:List[Boolean]):Boolean = foldRight(lst,true)(_&&_)

  def takewhile[A](lst:List[A])(p:A=>Boolean):List[A] = foldRight(lst,Nil:List[A])((h,t) => if (p(h)) Const(h,t) else Nil)

  def filter[A](lst:List[A])(p:A=>Boolean):List[A] = foldRight(lst,Nil:List[A])((h,t) => if (p(h)) Const(h,t) else t)

  //TODO
  //def unzipFR[A,B](lst:List[A]):(List[A],List[B]) = foldRight(lst,(Nil,Nil):(List[A],List[B]))((h,t)=> (Const(h._1,t._1),Const(h._2,t._2)))

  def lengthFL[A](lst:List[A]):Int = foldLeft(lst,0)((h,_)=>h+1)

  def andFL(lst:List[Boolean]):Boolean = foldLeft(lst,true)(_&&_)

  //TODO
  //def takewhileFL[A](lst:List[A])(p:A=>Boolean):List[A] = foldLeft(lst,Nil:List[A])((h,t) => if (p(t)) addEnd(h,t) else reverse(drop(1,h))  )

  def filterFL[A](lst:List[A])(p:A=>Boolean):List[A] = foldLeft(lst,Nil:List[A])((h,t) => if (p(t)) addEnd(h,t) else h)

  //TODO
  //def unzipFL[A,B](lst:List[A]):(List[A],List[B]) = foldLeft(lst,(Nil,Nil):(List[A],List[B]))((h,t)=> (addEnd(h._1,t._1),addEnd(h._2,t._2)))

  def apply[A](as: A*) : List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail:_*))
  }

}