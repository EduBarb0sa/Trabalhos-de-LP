{-
   QUESTÃO 7  

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

inserirMedicamento :: Medicamento -> Horario -> PlanoMedicamento -> PlanoMedicamento
inserirMedicamento med hor [] = [(hor, [med])]
inserirMedicamento med hor ((h, meds):resto)
    | hor == h = (h, med:meds) : resto
    | hor < h = (hor, [med]) : (h, meds) : resto
    | otherwise = (h, meds) : inserirMedicamento med hor resto

converteReceituarioParaPlano :: Receituario -> PlanoMedicamento
converteReceituarioParaPlano [] = []
converteReceituarioParaPlano ((med, horarios):resto) =
    insereHorarios med horarios (converteReceituarioParaPlano resto)
  where
    insereHorarios :: Medicamento -> [Horario] -> PlanoMedicamento -> PlanoMedicamento
    insereHorarios _ [] plano = plano
    insereHorarios med (h:hs) plano = insereHorarios med hs (inserirMedicamento med h plano)

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receituario = converteReceituarioParaPlano receituario

-- Check 100%