{-
   QUESTÃO 7  

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

inserirMedicamentoOrdenado :: (Horario, [Medicamento]) -> PlanoMedicamento -> PlanoMedicamento
inserirMedicamentoOrdenado novo [] = [novo]
inserirMedicamentoOrdenado novo@(hor, meds) ((h, m):resto)
    | hor < h = novo : (h, m) : resto
    | hor == h = (h, meds ++ m) : resto
    | otherwise = (h, m) : inserirMedicamentoOrdenado novo resto

converteReceituarioParaPlano :: Receituario -> PlanoMedicamento
converteReceituarioParaPlano [] = []
converteReceituarioParaPlano ((med, horarios):resto) =
    insereHorarios med horarios (converteReceituarioParaPlano resto)
  where
    insereHorarios :: Medicamento -> [Horario] -> PlanoMedicamento -> PlanoMedicamento
    insereHorarios _ [] plano = plano
    insereHorarios med (h:hs) plano = insereHorarios med hs (inserirMedicamentoOrdenado (h, [med]) plano)

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receituario = converteReceituarioParaPlano receituario

-- Check 100%