-- Função para adicionar um horário à lista de horários de um medicamento no receituário
adicionarHorario :: Medicamento -> Horario -> Receituario -> Receituario
adicionarHorario med hor [] = [(med, [hor])]
adicionarHorario med hor ((m, horarios):resto)
    | med == m = (m, insereOrdenadoHorario hor horarios) : resto
    | med < m = (med, [hor]) : (m, horarios) : resto
    | otherwise = (m, horarios) : adicionarHorario med hor resto
  where
    insereOrdenadoHorario :: Horario -> [Horario] -> [Horario]
    insereOrdenadoHorario h [] = [h]
    insereOrdenadoHorario h (x:xs)
        | h == x = x:xs
        | h < x = h:x:xs
        | otherwise = x : insereOrdenadoHorario h xs

-- Função para converter plano de medicamentos em receituário
convertePlanoParaReceituario :: PlanoMedicamento -> Receituario
convertePlanoParaReceituario [] = []
convertePlanoParaReceituario ((hor, meds):resto) =
    insereMedicamentos meds hor (convertePlanoParaReceituario resto)
  where
    insereMedicamentos :: [Medicamento] -> Horario -> Receituario -> Receituario
    insereMedicamentos [] _ receituario = receituario
    insereMedicamentos (m:ms) hor receituario = insereMedicamentos ms hor (adicionarHorario m hor receituario)

-- Função principal para gerar o receituário a partir do plano de medicamentos
geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano plano = convertePlanoParaReceituario plano