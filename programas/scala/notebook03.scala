// Imports 
import scala.math._
import scala.annotation.tailrec













// Ejercicio 1 (factorial)

def factorial(n:Int):Int = {
  @tailrec
  def iFactorial(n:Int, prod:Int):Int = n match {
    case 0 => prod
    case n => iFactorial(n - 1, n * prod)
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










// Ejercicio 3 (int2String)

def int2String(i:Int):List[Char] = {
  def iInt2String(i:Int,r:List[Char]):List[Char] = {
    if (i != 0) {
      val d = i % 10
      val c = (d + '0').toChar
      iInt2String(i / 10, c +: r)
    }
    else r
  }
  if (i == 0) List('0')
  else if (i < 0) '-' +: iInt2String(abs(i),List())
  else iInt2String(i,List())
}




// Ejercicio 4 (sumList)

def sumList(lst:List[Int]):Int = {
  def iSumList(lst:List[Int], acum:Int):Int = {
    if (lst.isEmpty) acum
    else iSumList(lst.tail, acum + lst.head)
  }
  iSumList(lst, 0)
}


