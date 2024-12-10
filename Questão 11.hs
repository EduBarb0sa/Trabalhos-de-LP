atualizaEstoque :: [(String, Int)] -> Cuidado -> Maybe [(String, Int)]
atualizaEstoque estoque (Medicar med) = 
    case lookup med estoque of
        Just q -> if q > 1 then Just ((med, q - 1) : filter ((/= med) . fst) estoque)
                 else Just (filter ((/= med) . fst) estoque)
        Nothing -> Nothing
atualizaEstoque estoque (Comprar med q) = 
    case lookup med estoque of
        Just qAtual -> Just ((med, qAtual + q) : filter ((/= med) . fst) estoque)
        Nothing -> Just ((med, q) : estoque)

processaPlantao :: [(String, Int)] -> [(Int, [Cuidado])] -> Maybe [(String, Int)]
processaPlantao estoque [] = Just estoque
processaPlantao estoque ((_, acoes):ps) = 
    case foldl (\acc acao -> acc >>= (`atualizaEstoque` acao)) (Just estoque) acoes of
        Just novoEstoque -> processaPlantao novoEstoque ps
        Nothing -> Nothing

verificaPlano :: [(Int, [String])] -> [(Int, [Cuidado])] -> Bool
verificaPlano [] _ = True
verificaPlano ((horario, meds):ps) plantao = 
    let ministrados = ministradosNoHorario horario plantao
        resultado = all (`elem` ministrados) meds
    in resultado && verificaPlano ps plantao

ministradosNoHorario :: Int -> [(Int, [Cuidado])] -> [String]
ministradosNoHorario h plantao = 
    concat [ [med | Medicar med <- acoes] | (hora, acoes) <- plantao, hora == h ]

satisfaz :: [(Int, [Cuidado])] -> [(Int, [String])] -> [(String, Int)] -> Bool
satisfaz plantao plano estoque = 
    case processaPlantao estoque plantao of
        Just estoqueFinal -> not (null estoqueFinal) && verificaPlano plano plantao
        Nothing -> False

plantaoCorreto :: [(Int, [String])] -> [(String, Int)] -> [(Int, [Cuidado])]
plantaoCorreto plano estoque = snd $ foldl criarCuidado (estoque, []) plano
  where
    criarCuidado (est, plantao) (horario, meds) = 
        let (novoEst, acoes) = foldl (adicionarCuidado horario) (est, []) meds
        in (novoEst, plantao ++ [(horario, acoes)])
    
    adicionarCuidado horario (est, acoes) med = 
        case lookup med est of
            Just q -> if q > 0 
                      then ((med, q - 1) : filter ((/= med) . fst) est, acoes ++ [Medicar med])
                      else ((med, 1) : est, acoes ++ [Comprar med 1, Medicar med])
            Nothing -> ((med, 1) : est, acoes ++ [Comprar med 1, Medicar med])