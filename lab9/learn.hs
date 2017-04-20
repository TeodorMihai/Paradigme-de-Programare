data Person = Person { name :: String, cnp :: Integer }

instance Eq Person where
	Person name1 cnp1 == Person name2 cnp2 = cnp1 == cnp2
	p1 /= p2 = not (p1 == p2)