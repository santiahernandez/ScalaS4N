package week1
package co.Inmutable

import scala.annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

object List {
  def lenght[A](lst:List[A]):Int =  lst match{
    case Nil => 0
    case Const(h,t) => 1 + lenght(t)
  }

  def tail[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case Const(h,Nil) => Nil
    case Const(h,t) => t
  }

  def head[A](lst:List[A]):A = lst match {
    case Const(h,Nil) => h
    case Const(h,t) => h
  }


  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Const(h,t) => h + sum(t)
  }

  @tailrec
  def and(lst:List[Boolean]):Boolean = lst match {
    case Const(true,Nil) => true
    case Const(true,t) => and(t)
    case Const(false,_) => false
  }

  @tailrec
  def or(lst:List[Boolean]):Boolean = lst match {
    case Const(true,Nil) => true
    case Const(false,Nil) => false
    case Const(false,t) => or(t)
    case Const(true, t) => true
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
    def maxDouble (ints:List[Double]):Double = {
      def maximDouble (a:Double , b:Double):Double = {
        if (a>=b) a
        else b
      }
      ints match {
        case Const(h,Nil) => h
        case Const(h,t) => maximDouble(h,maxDouble(t))
      }
    }
    def minDouble (ints:List[Double]):Double = {
      def minimDouble (a:Double , b:Double):Double = {
        if (a>=b) b
        else a
      }
      ints match {
        case Const(h,Nil) => h
        case Const(h,t) => minimDouble(h,minDouble(t))
      }
    }
    (minDouble(lst),maxDouble(lst))
  }


  def apply[A](as: A*) : List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail:_*))
  }
}