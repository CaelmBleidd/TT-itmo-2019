import Data.Fin

%default total

weakenN2 : (n : Nat) -> Fin m -> Fin (n + m)
weakenN2 Z f = f
weakenN2 (S k) f = weaken (weakenN2 k f)

plus_fin : Fin m -> Fin n -> Fin (m + n)
plus_fin {m = S p} FZ t = FS (weakenN2 p t)
plus_fin {m = S m} {n = p} (FS f) l = FS {k = (m + p)} (plus_fin f l)

mul_fin : Fin m -> Fin n -> Fin (m * n)
mul_fin {m=S x} {n=S y} FZ l = weakenN (mult x (S y)) l
mul_fin (FS l) f = plus_fin f (mul_fin l f)

main : IO()
main = putStrLn $ "" --  (plus_fin (FS(FS(FS(FS(FZ {k = 6}))))) (FS(FS(FZ {k = 4}))))
