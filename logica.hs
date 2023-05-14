nand:: Bool->Bool->Bool
nand True True = False
nand _ _ = True

-- ---------------------------------------------------- 
-- maj retorna True sii al menos 2 argumentos son True.
-- ----------------------------------------------------
maj :: Bool −> Bool −> Bool −> Bool
maj True True _ = Ture
maj True _ True = Ture
maj _ True True = Ture


-- ---------------------------------------------------- 
-- Para las siguientes funciones se debe respetar el 
-- perfil propuesto.
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a].
-- Mientras que (Int -> [a] -> Bool) es la propiedad.
--		Ejemplo: paraTodo [0,1,2,3] [4,1,2,6] even 
--		retorna False, ya que existe una posición 
--		en la que el elemento de la lista es impar. 
--		paraTodo [0,2,4,6] [2,2,4,4,4,5,6] even  
--		retorna True.
-- ----------------------------------------------------

paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo [] [] f = False
paraTodo [] ys f = False
paraTodo xs [] f = False
paraTodo (x:xs) (y:ys) f = and (foldl (&&) [map f x xs ++ map f x ys])

-- ----------------------------------------------------
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a]. 
-- (Int -> [a] -> Bool) es la propiedad.
--
--		Ejemplo: existe [0,1,2,3] [4,1,2,6] odd
--		retorna True.
-- ----------------------------------------------------
existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo [] [] f = Falso
paraTodo [] ys f = False
paraTodo xs [] f = False
paraTodo xs ys f = or (foldl (&&) [(map f x xs) ++ (map f x ys)])



