
data Codificador = Cifra [(Char, Char)] String 
-- utilizo el string para poder almacenar el string que voy 
-- cifrando

-- Eficiencia O(1)
nuevaC :: [(Char, Char)] -> Codificador
nuevaC tabla = (Cifra tabla [])

-- Eficiencia O(n log T)
codificarC :: Codificador -> String -> String
codificarC (Cifra pars str) []           = str
codificarC (Cifra pars str) (char:chars) =
	codificarC (Cifra pars (cifrarChar char pars str)) cx

-- Eficiancia O(n)
cifrarChar :: Char -> [(Char, Char)] -> String -> String
cifrarChar char [] str     = str
cifrarChar char (p:ps) str =
	if char == (fst p)
		then (snd p) : str
		else if char == (snd p)
				then (fst p) : str
				else cifrarChar char ps str