import scala.annotations._

def binomial(n:Int,k:Int):Int = {
  if (k == 0 || n == k) 1
  else binomial(n-1,k-1) + binomial(n-1,k)
}

def binomial2(n:Int,k:Int):Int = k match {
  case 0 => 1
  case k if (k == n) => 1
  case _ => binomial(n-1,k-1) + binomial(n-1,k)
}

def binomial3(n:Int,k:Int):Int = k match {
  case 0 => 1
  case k => if (k == n) 1
            else binomial(n-1,k-1) + binomial(n-1,k)
}

