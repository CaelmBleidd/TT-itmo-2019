module Main

import Data.Vect

%default total

get : (Fin n) -> Vect n a -> a
get (FS k) (x::xs) = get k xs
get FZ     (x::xs) = x

replaceAtPosition : Fin len -> elem -> Vect len elem -> Vect len elem
replaceAtPosition FZ     y (x::xs) = y :: xs
replaceAtPosition (FS k) y (x::xs) = x :: replaceAtPosition k y xs

swap : Vect n a -> (Fin n) -> (Fin n) -> Vect n a
swap vect a b = replaceAtPosition b first (replaceAtPosition a second vect)
  where
    first  = get a vect
    second = get b vect


main : IO ()
main = do
  let vect = Vect.fromList [1, 2, 3, 4, 5, 6, 7]
  putStrLn (show $ swap vect 2 5)


