my_gcd :: (Integral a) => a -> a -> a
my_gcd x y 
  | y > x     = gcd y x
  | r == 0    = y
  | otherwise = gcd y y
  where 
    r = x `mod` y

