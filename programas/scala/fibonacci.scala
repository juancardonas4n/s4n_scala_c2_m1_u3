def fibonacciIt(n:Int):Int = {
  var num1 = 0
  var num2 = 1
  var sum  = 0
  var i = 1
  while (i < n) {
    sum = num1 + num2
    num1 = num2
    num2 = sum
    i += 1
  }
  sum
}

def fibonacciRec(n:Int):Int = n match {
  case 0 => 0
  case 1 => 1
  case n => fibonacciRec(n-1)+fibonacciRec(n-2)
}

def fibRecCola(n:Int) = {
  def iFibRecCola(n:Int,t:(Int,Int)):Int = n match {
    case 0 => t._1
    case 1 => t._2
    case n => iFibRecCola(n-1,(t._2,t._1 + t._2))
  }
  iFibRecCola(n,(0,1))
}

def eval(n:Int):Unit =  {
  val timeRec = System.nanoTime
  fibonacciRec(n)
  val diffTimeRec = System.nanoTime - timeRec
  println(s"Tiempo total recursiva: $diffTimeRec")
  val timeIt = System.nanoTime
  fibonacciIt(n)
  val diffTimeIt = System.nanoTime - timeIt
  println(s"Tiempo total iterativa: $diffTimeIt")
  val aumento = diffTimeRec / diffTimeIt
  println(s"Aumento: $aumento")
}

def doEvals():Unit = {
  for (i <-0 to 40) {
    println(s"Eval $i")
    eval(i)
  }
}
