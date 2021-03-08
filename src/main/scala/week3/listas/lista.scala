package week3.listas

import scala.::
import scala.annotation.tailrec


object lista extends App {


  def removeSplit[A](lst:List[A], i:Int):List[A] = lst.splitAt(i) match {
    case (l1,l2) => l1.appendedAll(l2.tail)
  }

  def subs[A](lst:List[A]):List[List[A]] = lst match {
    case Nil => List(Nil)
    case h :: rest => subs(rest) ::: subs(rest).map(h :: _)
  }

  @tailrec
  def mylast[A](list: List[A]):A = list match {
    case x :: Nil => x
    case _ :: y  => mylast(y)
  }

  @tailrec
  def myButLast[A](list: List[A]):A = list match {
    case x :: _ :: Nil => x
    case _ :: y  => myButLast(y)
  }

  @tailrec
  def my2Last[A](list: List[A]):List[A] = list match {
    case x :: y :: Nil => List(x,y)
    case _ :: y  => my2Last(y)
  }

  @tailrec
  def kthElem[A](n:Int, lst:List[A]):A = (n,lst)match {
    case (1,head :: _) => head
    case (n,_:: tail)  => kthElem(n-1,tail)
  }

  def myLenght[A](lst:List[A]):Int = lst match {
    case Nil => 0
    case _::tail  => 1 + myLenght(tail)
  }

  def myLenghtFR[A](lst:List[A]):Int = lst.foldRight(0)((_,y)=>1+y)

  def myLenghtFL[A](lst:List[A]):Int = lst.foldLeft(0)((y,_)=>1+y)

  def reverseFR[A](lst:List[A]):List[A] = lst.foldRight(Nil:List[A])((head,aux)=>aux.appended(head))

  def reverseFL[A](lst:List[A]):List[A] = lst.foldLeft(Nil:List[A])((aux,head)=>List(head).appendedAll(aux))

  def palindrome[A](lst:List[A]):Boolean = {
    @tailrec
    def palindromeaux(lst:List[A], aux:List[A], counter:Int):Boolean = (lst,counter) match {
      case (Nil,0) => true
      case (x :: Nil,0) => true
      case (head :: tail, counter) => if(head ==tail.last) palindromeaux(tail.splitAt(tail.size-1)._1,aux,counter-1) else false
    }
    palindromeaux(lst,Nil:List[A],lst.size/2)
  }

  def compress[A](lst:List[A]):List[A] = lst.foldLeft(Nil:List[A])((aux,head)=> if (!aux.contains(head)) aux.appended(head) else aux)

  def myhead[A](list: List[A]):A = list match {
    case x :: _ => x
  }

  def pack [A](list: List[A]):List[List[A]] = {
    @tailrec
    def packaux(list: List[A], aux:List[List[A]], aux2:List[A]):List[List[A]] = (list,aux2) match {
      case (Nil,Nil) => aux
      case (Nil,cola )=> aux.appended(cola)
      case (x :: z,Nil) => packaux(z,aux, aux2:+x)
      case (x :: z, cola)  => if (myhead(cola) == x) packaux(z,aux, aux2:+x) else packaux(z,aux:+aux2,List(x))
    }
    packaux(list,Nil,Nil)
  }

  def encode [A](list: List[A]):List[(Int,A)] = {
    @tailrec
    def encodeaux(list: List[A], aux:List[(Int,A)], aux2:(Int,A)):List[(Int,A)] = (list,aux2) match {
      case (Nil,(0,_)) => aux
      case (Nil,(n,a) )=> aux.appended((n,a))
      case (x :: z, (_,a))  => if (a==x) encodeaux(z,aux, (aux2._1+1,a)) else encodeaux(z,aux.appended(aux2),(1,x))
    }
    encodeaux(list,Nil,(0,list.head))
  }

  def encodeModified[A](list: List[A]):List[(Boolean,Int,A)] = {
    @tailrec
    def encodeaux(list: List[A], aux:List[(Boolean,Int,A)], aux2:(Boolean,Int,A)):List[(Boolean,Int,A)] = (list,aux2) match {
      case (Nil,(_,0,_)) => aux
      case (Nil,(b,n,a) )=> aux.appended((b,n,a))
      case (x :: z, (_,_,a))  => if (a==x) encodeaux(z,aux, (true,aux2._2+1,a)) else encodeaux(z,aux.appended(aux2),(false,1,x))
    }
    encodeaux(list,Nil,(false,0,list.head))
  }

  def decode[A](list:List[(Boolean,Int,A)]): List[A] ={
    @tailrec
    def toList(int: Int, a:A, list: List[A]):List[A] = (int,list) match {
      case (0,lst) => lst
      case (n,lst) => toList(n-1,a,lst.appended(a))
    }

    @tailrec
    def decodeAux(list: List[(Boolean,Int,A)], aux:List[List[A]]):List[List[A]] = (list,aux) match {
      case (Nil,_) => aux
      case (x::tail, _) => decodeAux(tail,aux.appended(toList(x._2,x._3,Nil)))
    }
    decodeAux(list,Nil).flatten
  }

  def duplicate[A](list: List[A]):List[A] = list.foldLeft(Nil:List[A])((tail,head)=>tail:+head:+head)

  def replicate[A](list: List[A], n:Int):List[A] = {
    @tailrec
    def concatn(a:A, n:Int, aux:List[A]):List[A]= n match {
      case 0 => aux
      case n => concatn(a,n-1,aux)
    }

    @tailrec
    def replicateAux(list:List[A], n:Int, aux:List[List[A]]):List[List[A]] = (list,n) match {
      case (Nil,n) => aux
      case (head::tail,n) => replicateAux(tail, n, aux.appended(concatn(head,n,Nil)))
    }

    replicateAux(list,n,Nil).flatten
  }

  def dropEveryN [A](list: List[A], index:Int):List[A] = {
    @tailrec
    def dropAux(list:List[A], index:Int, aux:List[A], contador:Int):List[A] = (contador,list) match {
      case (_,Nil) => aux
      case (1,_::tail) => dropAux(tail,index,aux,index)
      case(_,head::tail) => dropAux(tail,index,aux:+head, contador-1)
    }
    dropAux(list,index,Nil,index)
  }


  val lst = List(1,1,4,5,4,4,4,4,2)


  println(pack(lst))
  println(encode(lst))
  println(encodeModified(lst))
  println(decode(encodeModified(lst)))
  println(duplicate(lst))
  println(replicate(lst,2))
}
