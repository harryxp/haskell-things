import Unsafe.Coerce (unsafeCoerce)

c g = \n -> if n == 0 then 1 else n * g (n - 1)

y f = (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))

fix f = f (fix f)

main =
  putStr "y c 5 = " >>
  putStrLn (show (y c 5)) >>
  putStr "fix c 5 = " >>
  putStrLn (show (fix c 5))

