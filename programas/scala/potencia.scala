

def potencia(a:Double,n:Int) = {
  def iPotencia(a:Double,n:Int,r:Double):Double = n match {
    case 0 => r
    case n => iPotencia(a,n-1,r * a)
  }
  iPotencia(a,n,1.0)
}
