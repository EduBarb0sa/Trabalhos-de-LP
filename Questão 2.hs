{-
   QUESTÃO 2,

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento med estoque = 
    let novoEstoque = decrementaQuantidade med estoque
    in if any (\(m, q) -> m == med && q > 0) novoEstoque
       then Just novoEstoque
       else Nothing

decrementaQuantidade :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
decrementaQuantidade _ [] = []
decrementaQuantidade med ((m, q):resto)
  | med == m && q > 1 = (m, q - 1) : resto
  | med == m && q == 1 = resto
  | otherwise = (m, q) : decrementaQuantidade med resto

-- Check 100%