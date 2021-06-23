// Imports 
import scala.math._
import scala.annotation.tailrec














// Ejercicio 1

def factorial(n:Int):Int = {
  @tailrec
  def iFactorial(n:Int, a:Int):Int = n match {
    case 0 => a
    case n => iFactorial(n - 1, n * a)
  }
  iFactorial(n,1)
}











// Ejercicio 2

def incr(n:Int) = n + 1
def decr(n:Int) = n - 1

@tailrec
def sumRec(a:Int, b:Int):Int = (a,b) match {
  case (a,0) => a
  case (0,b) => b
  case (a,b) => sumRec(incr(a),decr(b))
}










// Ejercicio 3
def int2String(i:Int):List[Char] = {
  @tailrec
  def iInt2String(i:Int,r:List[Char]):List[Char] = {
    if (i != 0) {
      val d = i % 10
      val c = (d + '0').toChar
      iInt2String(i / 10, c +: r)
    }
    else r
  }
  if (i == 0) List('0')
  else if (i < 0) '-' +: iInt2String(abs(i), List())
  else iInt2String(i, List())
}




// Ejercicio 4
def sumList(lst:List[Int]):Int = {
  @tailrec
  def iSumList(lst:List[Int],acum:Int):Int = {
    if (lst.isEmpty) acum
    else iSumList(lst.tail, acum + lst.head)
  }
  iSumList(lst,0)
}

def sumList2(lst:List[Int]):Int = {
  @tailrec
  def iSumList(r:(List[Int],Int)):Int = {
    if (r._1.isEmpty) r._2
    else iSumList(r._1.tail,r._1.head + r._2)
  }
  iSumList((lst,0))
}

def sumList3(lst:List[Int]):Int = {
  @tailrec
  def iSumList(lst:List[Int],acum:Int):Int = lst match {
    case Nil => acum
    case _   => iSumList(lst.tail, lst.head + acum)
  }
  iSumList(lst,0)
}

def sumList4(lst:List[Int]):Int = {
  @tailrec
  def iSumList(lst:List[Int],acum:Int):Int = lst match {
    case lst if lst.isEmpty => acum
    case _   => iSumList(lst.tail, lst.head + acum)
  }
  iSumList(lst,0)
}

