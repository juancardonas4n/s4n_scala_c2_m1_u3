
def gcd(m:Int,n:Int):Int = {
  if ((m % n) == 0) n
  else gcd(n, m % n)
}
