import scala.annotation.tailrec

def potenciaIt(a:Double, n:Int):Double = {
  var res = 1.0
  var i = 0;
  while (i < n) {
    res *= a
    i += 1
  }
  res
}

def potenciaRec(a:Double, n:Int):Double = n match {
  case 0 => 1
  case n => a * potenciaRec(a,n-1)
}

def potenciaTailRec(a:Double, n:Int):Double = {
  @tailrec def iPotencia(a:Double, n:Int, acum:Double):Double = n match {
    case 0 => acum
    case n => iPotencia(a,n-1,a*acum)
  }
  iPotencia(a,n,1.0)
}


def eval(a:Double, n:Int):Unit =  {
  val timeRec = System.nanoTime
  potenciaRec(a,n)
  val diffTimeRec = System.nanoTime - timeRec
  println(s"Tiempo total recursiva: $diffTimeRec")
  val timeTailRec = System.nanoTime
  potenciaTailRec(a,n)
  val diffTimeTailRec = System.nanoTime - timeTailRec
  println(s"Tiempo total recursiva de cola: $diffTimeTailRec")
  val mejora = diffTimeRec / diffTimeTailRec
  println(s"Porcentaje de mejora: $mejora")
  val timeIt = System.nanoTime
  potenciaIt(a, n)
  val diffTimeIt = System.nanoTime - timeIt
  println(s"Tiempo total iterativa: $diffTimeIt")
  val aumento = diffTimeRec / diffTimeIt
  println(s"Aumento: $aumento")
}

def doEvals():Unit = {
  for (i <- 0 to 40) {
    println(s"2^$i")
    eval(2,i)
  }
}
