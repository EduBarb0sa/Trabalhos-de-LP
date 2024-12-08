{-

   QUESTÃO 1

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qtd [] = [(med, qtd)]
comprarMedicamento med qtd estoque@(x:xs)
  | any (\(m, _) -> m == med) estoque = map (\(m, q) -> if m == med then (m, q + qtd) else (m, q)) estoque
  | otherwise = (med, qtd) : estoque


-- Check 100%