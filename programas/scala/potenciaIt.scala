def potenciaIt(a:Double, n:Int):Double = {
  var res = 1.0
  for (i <- 2 to n) res *= a
  res
}

def potencia(a:Double, n:Int):Double = n match {
  case 0 => 1
  case n => a * potencia(a,n-1)
}

def eval(a:Double, n:Int):Unit =  {
  val timeRec = System.nanoTime
  potencia(a,n)
  val diffTimeRec = System.nanoTime - timeRec
  println(s"Tiempo total recursiva: $diffTimeRec")
  val timeIt = System.nanoTime
  potenciaIt(a, n)
  val diffTimeIt = System.nanoTime - timeIt
  println(s"Tiempo total iterativa: $diffTimeIt")
  val aumento = diffTimeRec / diffTimeIt
  println(s"Aumento: $aumento")
}

