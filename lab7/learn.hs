factorial_pm 0 = 1
factorial_pm x = x * factorial_pm (x - 1)

square x = x * x

inc x = x + 1

f3 x  = inc . square $ x
{-
p = let x = y + 1
        y = 2
        b n = if n == 0 then [] else n : b (n - 1)
  in (x + y, b 2)
  -}

p = let x = y + 1
	y = 2
	f n = if n == 0 then [] else n : f ( n - 1 )
	in (x + y, f 2)

qsort [] = []
qsort (p : xs) = qsort left ++ [p] ++ qsort right 
	where 
	left = filter (< p) xs
	right = filter (>= p) xs
{-
naturals = iter 0
	where iter x = x : iter ( x + 1 )
-}
--itearet definita ulterior, primeste functie si numarul intiial si intoarce lista 
--cu elementele f aplicate
naturals = iterate ( \ x -> x + 1) 0
ones = repeat 5 
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
powerOfTwo = iterate ( \ x -> 2 * x) 1

--length ( tail ( zip [1, 2, 3, 4] "abcd" ) )

{--

>t ($)
($) :: (a -> b) -> a -> b

$ este operatorul care aplică o funcție unară pe parametrul său. 
Semantica expresiei f $ x este aceeași cu cea a f x,
 diferența fiind pur sintactică: precedența $ impune ordinea de evaluare
  astfel încât expresia din stânga $ este aplicată pe cea din dreapta
--
> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

. primeste practic 3 argumente: 2 functii
}
