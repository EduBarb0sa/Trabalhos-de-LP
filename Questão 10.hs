{-

QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

atualizaEstoque2 :: EstoqueMedicamentos -> Cuidado -> Maybe EstoqueMedicamentos
atualizaEstoque2 estoque (Medicar med) = 
    case lookup med estoque of
        Just q -> if q > 1 then Just ((med, q - 1) : filter ((/= med) . fst) estoque)
                 else Just (filter ((/= med) . fst) estoque)
        Nothing -> Nothing
atualizaEstoque2 estoque (Comprar med q) = 
    case lookup med estoque of
        Just qAtual -> Just ((med, qAtual + q) : filter ((/= med) . fst) estoque)
        Nothing -> Just ((med, q) : estoque)

processaPlantao :: EstoqueMedicamentos -> Plantao -> Maybe EstoqueMedicamentos
processaPlantao estoque [] = Just estoque
processaPlantao estoque ((horario, acoes):ps) = 
    case foldl (\acc acao -> acc >>= (`atualizaEstoque2` acao)) (Just estoque) acoes of
        Just novoEstoque -> processaPlantao novoEstoque ps
        Nothing -> Nothing

verificaPlano :: PlanoMedicamento -> Plantao -> Bool
verificaPlano [] _ = True
verificaPlano ((horario, meds):ps) plantao = 
    let ministrados = ministradosNoHorario horario plantao
        resultado = all (`elem` ministrados) meds
    in resultado && verificaPlano ps plantao

ministradosNoHorario :: Horario -> Plantao -> [Medicamento]
ministradosNoHorario h plantao = 
    concat [ [med | Medicar med <- acoes] | (hora, acoes) <- plantao, hora == h ]

-- Função principal
satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque = 
    case processaPlantao estoque plantao of
        Just estoqueFinal -> not (null estoqueFinal) && verificaPlano plano plantao
        Nothing -> False

-- Check/2 50%