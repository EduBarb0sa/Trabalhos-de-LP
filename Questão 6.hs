{-

   QUESTÃO 6

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}
plantaoValido :: Plantao -> Bool
listaOrdenadaEDistinta :: Ord a => [a] -> Bool
listaOrdenadaEDistinta [] = True
listaOrdenadaEDistinta [_] = True
listaOrdenadaEDistinta (x:y:xs) = x < y && listaOrdenadaEDistinta (y:xs)

semCompraMedicarMesmoHorario :: [Cuidado] -> Bool
semCompraMedicarMesmoHorario [] = True
semCompraMedicarMesmoHorario (x:xs) = casoValido x xs && semCompraMedicarMesmoHorario xs
  where
    casoValido _ [] = True
    casoValido (Comprar med1 _) (Medicar med2:resto) = med1 /= med2 && casoValido (Comprar med1 0) resto
    casoValido (Medicar med1) (Comprar med2 _:resto) = med1 /= med2 && casoValido (Medicar med1) resto
    casoValido _ (_:resto) = True

medicarOrdenado :: [Cuidado] -> Bool
medicarOrdenado [] = True
medicarOrdenado cuidados = let meds = [med | Medicar med <- cuidados] in listaOrdenadaEDistinta meds

plantaoValido plantao =
    horariosDistintosEOrdenados plantao &&
    all semCompraMedicarMesmoHorario (map snd plantao) &&
    all medicarOrdenado (map snd plantao)

horariosDistintosEOrdenados :: Plantao -> Bool
horariosDistintosEOrdenados [] = True
horariosDistintosEOrdenados [_] = True
horariosDistintosEOrdenados ((h1, cuidados1):(h2, cuidados2):resto) =
    h1 < h2 && horariosDistintosEOrdenados ((h2, cuidados2):resto)

-- Chech 100%