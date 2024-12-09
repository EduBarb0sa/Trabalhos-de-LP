{-
   QUESTÃO 4

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.
  
-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((med, horarios):resto) =
  inserirOrdenado (med, length horarios) (demandaMedicamentos resto)
inserirOrdenado :: (Medicamento, Quantidade) -> EstoqueMedicamentos -> EstoqueMedicamentos
inserirOrdenado novo [] = [novo]
inserirOrdenado novo@(med, qtd) ((m, q):resto)
  | med < m = novo : (m, q) : resto
  | med == m = (med, qtd + q) : resto 
  | otherwise = (m, q) : inserirOrdenado novo resto

-- Check 100%