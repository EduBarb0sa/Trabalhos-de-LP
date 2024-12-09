{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

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

-- Check 100%