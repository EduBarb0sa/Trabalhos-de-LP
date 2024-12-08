receituarioValido :: Receituario -> Bool
planoValido :: PlanoMedicamento -> Bool

listaOrdenadaEDistinta :: Ord a => [a] -> Bool
listaOrdenadaEDistinta [] = True
listaOrdenadaEDistinta [_] = True
listaOrdenadaEDistinta (x:y:xs) = x < y && listaOrdenadaEDistinta (y:xs)
--A
receituarioValido receituario =
    listaOrdenadaEDistinta (map fst receituario) &&
    all (listaOrdenadaEDistinta . snd) receituario
-- B
planoValido plano =
    listaOrdenadaEDistinta (map fst plano) &&
    all (listaOrdenadaEDistinta . snd) plano

-- Check 100%