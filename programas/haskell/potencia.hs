
potencia :: Double -> Integer -> Double
potencia a n = ipotencia a n 1
  where ipotencia _ 0 r  = r
        ipotencia a n r = ipotencia a (n - 1) (r * a)
