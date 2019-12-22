%default total

--2a
plusReducesZ : (n:Nat) -> n = plus n Z
plusReducesZ Z = Refl
plusReducesZ (S k) = cong (plusReducesZ k)

--cong : {f : t -> u} -> (a = b) -> f a = f b
--cong Refl = Refl

--2b
plusOne : (n: Nat) -> S n = plus 1 n
plusOne n = Refl

--2c
plusOne' : (n: Nat) -> S n = plus n 1
plusOne' Z = Refl
plusOne' (S k) = cong (plusOne' k)

--2f
plusReducesS : (n:Nat) -> (m:Nat) -> S (plus n m) = plus n (S m)
plusReducesS Z m = Refl
plusReducesS (S k) m = cong (plusReducesS k m)

--3a
mult_n0 : (n:Nat) -> (Z = (mult n Z))
mult_n0 Z = Refl
mult_n0 (S k) = mult_n0 k;

--3b
mult_n0' : (n:Nat) -> (Z = (mult Z n))
mult_n0' Z = Refl
mult_n0' (S k) = Refl

--3с
mult_one : (n: Nat) -> n = mult 1 n
mult_one Z = Refl
mult_one (S k) = cong (mult_one k)

--3d
mult_one' : (n: Nat) -> n = mult n 1
mult_one' Z = Refl
mult_one' (S k) = cong (mult_one' k)



main : IO()
main = putStrLn ""
