add1 x y = x + y
result1 = (+) 1 2  	

f x y = x + y

--legare dinamic
g = \x y -> x + y
--legarea statica
h x y = x + y
-- x = y , cand zic x zic de fapt y

--ca sa infixez un apel de functie
-- elem 2 [ 1, 2, 3 ] == 2 `elem` [ 1, 2, 3 ]
-- (2 +) = \x -> 2 + x
-- :: separa o expresie de tipul ei
-- constructori lsite : [] si (:)   aduga la inceputul listei

--list comprehension:

--take 5 f = [x | x <- [0, 2 ..], x `mod` 3 == 0]
factorial_if x = if x < 1 then 1 else x * factorial_if (x - 1)

factorial_guard x 
	| x < 1 = 1
	|otherwise = x * factorial_guard (x - 1)

factorial_case x = case x < 1 of
	True -> 1
	_ -> x * factorial_case ( x - 1 )

factorial_pm 0 = 1
factorial_pm x = x * factorial_pm (x-1)
--otherwise echivlaent cu true,  se exexuta orima garda adevarata

length_if l = if  l /= [] then 1 + length_if (tail l) else 0

length_guard l
	| l /= [] = 1 + length_guard (tail l)
	| otherwise = 0 

length_case l = case l of 
	(_ : xs) -> 1 + length_case (tail l)
	_ -> 0

length_pm [] = 0
length_pm x = 1 + length_pm ( tail x )
