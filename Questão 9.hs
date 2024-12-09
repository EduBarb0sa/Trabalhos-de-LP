atualizaEstoque :: Cuidado -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
atualizaEstoque (Comprar med qtd) estoque = Just (adicionarMedicamento med qtd estoque)
atualizaEstoque (Medicar med) estoque = consumirMedicamento med estoque

adicionarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
adicionarMedicamento med qtd [] = [(med, qtd)]
adicionarMedicamento med qtd ((m, q):resto)
    | med == m = (m, q + qtd) : resto
    | otherwise = (m, q) : adicionarMedicamento med qtd resto

consumirMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
consumirMedicamento med [] = Nothing
consumirMedicamento med ((m, q):resto)
    | med == m && q > 0 = Just ((m, q - 1) : resto)
    | med == m && q == 0 = Nothing
    | otherwise = case consumirMedicamento med resto of
        Nothing -> Nothing
        Just novoResto -> Just ((m, q) : novoResto)

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] estoque = Just estoque
executaPlantao ((_, cuidados):resto) estoque = executaCuidados cuidados estoque >>= executaPlantao resto

executaCuidados :: [Cuidado] -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaCuidados [] estoque = Just estoque
executaCuidados (c:cs) estoque = atualizaEstoque c estoque >>= executaCuidados cs