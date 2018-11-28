# Dados do relatório

# library(devtools)
# install_github("rpradosiqueira/sidrar")
library(sidrar)
library(dplyr)
library(data.table)
library(janitor)
library(tidyr)
library(scales)
library(readr)
library(stringr)
library(googledrive)
library(stringr)

#funçõezinhas que eu vou usar:

teste_igualdade_nomes_var_df <- function(base1, base2) {
  
  x <- names(base1)
  y <- names(base2)
  n <- length(x)
  k <- length(y)
  
  teste_nome_igual_x <- numeric()
  teste_nome_igual_y <- numeric()
  
  for ( i in 1:n) {
    teste_nome_igual_x[i] <- x[i] %in% y
  }
  
  for ( i in 1:k) {
    teste_nome_igual_y[i] <- y[i] %in% x
  }
  resp_x <- paste(x[!as.logical(teste_nome_igual_x)], collapse = ", ")
  resp_y <- paste(y[!as.logical(teste_nome_igual_y)], collapse = ", ")
  
  cat(paste("Colunas de", deparse(substitute(base1)), "ausentes em" , 
            deparse(substitute(base2)), ":", resp_x,
            ".\n\nColunas de", deparse(substitute(base2)), "ausentes em" ,
            deparse(substitute(base1)), ":", resp_y,
            sep=" "))
  
}

`%notin%` = function(x,y) !(x %in% y)

perc <- function(x) { 
  paste0(round(x,2)*100, "%")
}

#Obras de novembro coletadas no SIMEC do FNDE:
obras_novembro <- fread("obras21112018.csv",sep=";", colClasses = "character")

obras <- obras_novembro %>%
  clean_names()

# Filtros:
# Ids de controle:
load("ids_controle_final.Rdata") 

#retirando obras que não são construção de escolas e creches:

not_project<- c("COBERTURA DE QUADRA ESCOLAR - PROJETO PRÓPRIO",
                "COBERTURA DE QUADRA ESCOLAR GRANDE - PROJETO FNDE",
                "COBERTURA DE QUADRA ESCOLAR PEQUENA - PROJETO FNDE",
                "QUADRA ESCOLAR COBERTA - PROJETO PRÓPRIO ",
                "QUADRA ESCOLAR COBERTA COM VESTIÁRIO- PROJETO FNDE",
                "Reforma",
                "QUADRA ESCOLAR COBERTA - PROJETO PRÓPRIO",
                "Ampliação",
                "QUADRA ESCOLAR COBERTA COM PALCO- PROJETO FNDE",
                "Quadra Escolar Coberta e Vestiário - Modelo 2",
                "Ampliação Módulo Tipo B", 
                "")

#  Tempo de execução dos projetos-padrão (info obtida via LAI)

tempo_projeto <- data.frame(tipo_do_projeto = c("Escola de Educação Infantil Tipo B",
                                                "Escola de Educação Infantil Tipo C",
                                                "MI - Escola de Educação Infantil Tipo B",
                                                "MI - Escola de Educação Infantil Tipo C",
                                                "Espaço Educativo - 12 Salas",
                                                "Espaço Educativo - 01 Sala",
                                                "Espaço Educativo - 02 Salas",
                                                "Espaço Educativo - 04 Salas",
                                                "Espaço Educativo - 06 Salas",
                                                "Projeto 1 Convencional",
                                                "Projeto 2 Convencional",
                                                "Construção",
                                                "Escola com projeto elaborado pelo concedente",
                                                "Escola com Projeto elaborado pelo proponente",
                                                "Espaço Educativo - 08 Salas",
                                                "Espaço Educativo Ensino Médio Profissionalizante"),
                            tempo_execucao_dias = c(270,180,180,120,390,150,150,210,210,
                                                    330,270,720,720,720,720,720))

# Tempo de execução das obras estaduais, baseados nos cronogramas obtidos via LAI:

execucao_cronogramas_lai <- fread("tempo_obra.csv")
execucao_cronogramas_lai <- execucao_cronogramas_lai  %>%
  mutate(project_id = as.character(project_id),
         tempo_obra_dias = tempo_obra*30) %>%
  rename(tempo_obra_dias_via_lai = tempo_obra_dias) %>%
  select(1,3)

# Marcando as obras que também são parte do projeto Obra Transparente, da TBrasil:

load("ot.Rdata")

ot1 <- ot %>%
  mutate_all(as.character) %>%
  mutate(projeto_obra_transparente = 1)  %>%
  select(id_obra, projeto_obra_transparente )

#Crianco objeto geral:
# Objeto geral:
x <- as.Date("1900-01-01")

geral <- obras %>%
  filter(!id %in% id_controle_campanha_final,   #tirando os ids que estão no controle da campanha
         tipo_do_projeto %notin% not_project) %>% #apenas constru de esc e creches
  rename(responsabilidade = rede_de_ensino_publico) %>%
  left_join(execucao_cronogramas_lai, by=c("id" = "project_id")) %>% #infos que pegamos via lai
  left_join(tempo_projeto, by=c("tipo_do_projeto")) %>%      #tempo que o projeto padrão dura
  mutate(data_de_assinatura_do_contrato = as.Date(data_de_assinatura_do_contrato, format="%Y-%m-%d %H:%M:%S"),
         data_prevista_de_conclusao_da_obra= as.Date(data_prevista_de_conclusao_da_obra, format="%d/%m/%Y"),
         final_previsto = if_else(!is.na(data_prevista_de_conclusao_da_obra), data_prevista_de_conclusao_da_obra, 
                                  if_else(is.na(data_prevista_de_conclusao_da_obra) & 
                                            !is.na(tempo_obra_dias_via_lai), data_de_assinatura_do_contrato + tempo_obra_dias_via_lai, 
                                          if_else(is.na(data_prevista_de_conclusao_da_obra) & 
                                                    is.na(tempo_obra_dias_via_lai),
                                                  data_de_assinatura_do_contrato + tempo_execucao_dias, x))),
         tipo_data_final = ifelse(!is.na(data_prevista_de_conclusao_da_obra), "Data oficial",
                                  ifelse(is.na(data_prevista_de_conclusao_da_obra) & 
                                           !is.na(tempo_obra_dias_via_lai), "Estimada (data contrato + execução segundo pref)",
                                         ifelse(is.na(data_prevista_de_conclusao_da_obra) & 
                                                  is.na(tempo_obra_dias_via_lai) &
                                                  !is.na(tempo_execucao_dias), "Estimada (data contrato + execução cron padrão)", NA ))),
         # Status segundo TB
         percentual_de_execucao = as.numeric(percentual_de_execucao),
         nao_iniciada = ifelse( percentual_de_execucao == 0 & !situacao %in%
                                  c("Inacabada","Paralisada", "Obra Cancelada", "Concluída" ), 1, 0),
         paralisada_off = ifelse(situacao %in% c("Inacabada","Paralisada"), 1, 0),
         paralisada_nao_off = if_else(!is.na(data_de_assinatura_do_contrato) & situacao != "Execução" & nao_iniciada == 0|
                                        percentual_de_execucao > 0 & situacao != "Execução"  & nao_iniciada == 0|
                                        !is.na(data_prevista_de_conclusao_da_obra) & nao_iniciada == 0 & situacao %in% c("Licitação", "Em Reformulação","Contratação", 
                                                                                                                         "Planejamento pelo proponente"),
                                      1 , 0),
         paralisada_nao_off = ifelse(situacao %in% c("Obra Cancelada", "Concluída",
                                                     "Inacabada","Paralisada"), 0, paralisada_nao_off), #retirando concluidas e canceladas
         paralisada = ifelse(paralisada_nao_off == 1 | paralisada_off == 1, 1, 0),
         concluida = ifelse(situacao == "Concluída", 1, 0),
         cancelada = ifelse(situacao == "Obra Cancelada", 1, 0),
         atrasada = if_else(final_previsto < "2018-11-21" & situacao %notin% c("Concluída","Obra Cancelada"),
                            1, 0),
         atrasada = if_else(is.na(final_previsto), 0, atrasada),
         execucao = if_else(situacao == "Execução" & nao_iniciada == 0 , 1, 0),
         responsabilidade = as.character(responsabilidade),
         responsabilidade = if_else(id == "1063221" | id == "29054",
                                    "Municipal", responsabilidade),
         logradouro = tolower(logradouro),
         logradouro = str_trim(logradouro), # retirar espaços no fim
         logradouro = ifelse(logradouro=="", NA, logradouro),
         sem_end = if_else(is.na(logradouro), 1, 0), #obras que não tÊm endereço.
         #problema detectado:
         problema_detectado = ifelse(paralisada == 1 & sem_end == 1, #obras com problemas
                                     "paralisada; sem endereço",
                                     if_else(paralisada == 1 & atrasada == 1, "paralisada; atrasada",
                                             ifelse(paralisada == 1 & atrasada == 0, "paralisada",
                                                    ifelse(atrasada == 1 & sem_end == 1, "atrasada; sem endereço",
                                                           ifelse(atrasada == 1 & paralisada == 0, "atrasada",
                                                                  ifelse(sem_end == 1, "sem endereço",
                                                                         NA))))))) %>%
  #criando uma coluna de status para facilitar a vida:
  mutate(status = ifelse(paralisada == 1, "paralisada",
                         ifelse(cancelada == 1, "cancelada",
                                ifelse(nao_iniciada == 1, "não iniciada",
                                       ifelse(concluida == 1, "concluida",
                                              ifelse(execucao == 1, "execucao", "ERROOOOOOO"))))),
         situacao_segundo_tbrasil = ifelse(paralisada == 1 & atrasada == 0, "paralisada",
                                           ifelse(paralisada == 1 & atrasada == 1, "paralisada e já devia ter sido entregue",
                                                  ifelse(execucao == 1 & atrasada == 1, "em andamento e já devia ter sido entregue",
                                                         ifelse(execucao == 1 & atrasada == 0, "em andamento",
                                                                ifelse(concluida == 1, "obra concluída",
                                                                       ifelse(cancelada == 1, "obra cancelada",
                                                                              ifelse(nao_iniciada == 1 & atrasada == 0, "não iniciada",
                                                                                     ifelse(nao_iniciada == 1 & atrasada == 1,
                                                                                            "não iniciada e já devia ter sido entregue", "ERROOOOOO")))))))),
         logradouro = ifelse(is.na(logradouro), "Não informado", logradouro),
         ano_convenio = str_sub(termo_convenio, start= -4),
         ano_fim_vigencia_convenio = str_sub(fim_da_vigencia_termo_convenio, start= -4),
         ano_data_final_prevista_e_estimada = str_sub(final_previsto, 1, 4),
         fim_da_vigencia_termo_convenio = ifelse(fim_da_vigencia_termo_convenio == "", NA,
                                                 fim_da_vigencia_termo_convenio)) %>%
  select(id, nome, municipio, uf, responsabilidade, logradouro,percentual_de_execucao,
         ano_convenio, valor_pactuado_com_o_fnde, cancelada, concluida, paralisada_nao_off, paralisada_off, paralisada, nao_iniciada, execucao, atrasada, sem_end,
         status, situacao_segundo_tbrasil, situacao, ano_fim_vigencia_convenio, termo_convenio,
         data_prevista_de_conclusao_da_obra, final_previsto, tipo_data_final, ano_data_final_prevista_e_estimada, tipo_do_projeto) %>%
  rename(status_segundo_simec = situacao,
         data_final_prevista_e_estimada = final_previsto)  %>%
  left_join(ot1 , by=c("id" = "id_obra")) %>%  #vendo quais são os que estão no Obra Transparente
  mutate(projeto_obra_transparente = ifelse(is.na(projeto_obra_transparente), 0,
                                            projeto_obra_transparente))

# Juntando geral com dados financeiros:

# Para todas as obras cujo convênio contém apenas uma obra, foi utilizado o valor real, corrigido para o IPCA Set2018
# para as demais obras, o valor repassado foi estimado por um modelo estatístico em outro script:

# Dados da raspagem:
load("simec_fin.RData")

#Vou acertar o repasse com a inflação:
ipca_2012 <- get_sidra(x = 1419,
                       variable = 63,
                       geo = "Brazil",
                       period = c("201201-201810"),
                       classific = "c315",
                       category = list(7169),
                       header = TRUE,
                       format = 4) %>%
  clean_names() %>%
  select(brasil, variavel, mes_codigo, valor) %>%
  mutate(valor = valor/100)


ipca_2007 <- get_sidra(x = 2938,
                       variable = 63,
                       geo = "Brazil",
                       period = c("200701-201112"),
                       classific = "c315",
                       category = list(7169),
                       header = TRUE,
                       format = 4) %>%
  clean_names() %>%
  select(brasil, variavel, mes_codigo, valor) %>%
  mutate(valor = valor/100)

ipca <- bind_rows(ipca_2007, ipca_2012)

ipca1 <- ipca %>%
  mutate(ano = substr(mes_codigo, start=1, stop=4),
         mes = substr(mes_codigo, start=5, stop=6),
         mes_ano = paste(mes, ano, sep="/"),
         indice = cumprod(1+valor/100),    #funciona como juros compostos
         indice_max = last(indice),
         indice = indice/indice_max) %>% #ajeita o valor da multiplicação dependendo do ano
  select(mes_ano, indice)


repasses <- simec_fin2 %>%
  mutate(mes_ano = format(data_de_pagamento, "%m/%Y")) %>%
  left_join(ipca1, by=c("mes_ano")) %>%
  mutate(pagamento_cte_nov2018 = round(valor_do_pagamento/indice, 0)) %>%   #ajusta
  group_by(id) %>%
  summarise(pagto_total_cte_nov2018 = sum(pagamento_cte_nov2018),
            data_primeiro = min(data_de_pagamento),    #primeira data registrada do repasse
            data_ultimo_repasse = max(data_de_pagamento),      #última data registrada do repasse
            qtd_repasses = n())   

#corrigindo o valor pactuado com o FNDE, vou tratar sempre como junho do ano:

pacto <- geral %>%
  select(id, valor_pactuado_com_o_fnde, ano_convenio) %>%
  mutate(mes_ano = paste("06", ano_convenio, sep="/"),
         valor_fnde = as.numeric(valor_pactuado_com_o_fnde)) %>%
  left_join(ipca1, by=c("mes_ano")) %>%
  mutate(pactuado_cte_nov2018 = round(valor_fnde/indice, 0)) %>%
  select(id, ano_convenio, pactuado_cte_nov2018)

obras_repasses <- geral %>%
  left_join(repasses, by=c("id")) %>%
  mutate(qtd_repasses = ifelse(is.na(qtd_repasses), 0, qtd_repasses)) %>%
  select(-c(valor_pactuado_com_o_fnde, ano_convenio)) %>%
  left_join(pacto, by = c("id")) %>%
  mutate_all(as.character)

# Para convênios com mais de uma obra vamos estimar os repasses. Então: 

convenio_uma_obra <- obras_repasses %>%
  group_by(termo_convenio) %>%
  mutate(qtde_obras_convenio = n()) %>%
  filter(qtde_obras_convenio == 1) %>%
  mutate(estimativa_repasse = 0, 
         margem_erro_estimativa = 0)

# Juntando para os repasses estimados (quando há mais de uma obra):
load("repasses_estimados.Rdata")

sit_obras_final <- convenio_uma_obra %>%
  bind_rows(repasses_estimados) %>%
  mutate(pagto_total_cte_nov2018 = as.numeric(pagto_total_cte_nov2018),
         pagto_final = ifelse(estimativa_repasse == 0, pagto_total_cte_nov2018, estimativa_repasse),
         valor_estimado = ifelse(estimativa_repasse == 0, "não", "sim"))

save(sit_obras_final, file="sit_obras_final.Rdata")
write.csv(sit_obras_final , file="sit_obras_final.csv", sep=";", quote = TRUE,
          row.names = FALSE)

# arquivo que subiremos para enviar a ação no nosso sistema (irrelevante para quem quer apenas replicar o estudo,
# mas eu crio um objeto de filtro futuramente a partir desse objeto)

#Apenas as que receberão alertas:
envio_acao <- sit_obras_final %>%
  filter(!status %in% c("cancelada", "concluida")) %>%   #tem concluidas e canceladas sem end
  filter(nao_iniciada == 1 |
           paralisada_nao_off == 1 |
           paralisada_off == 1 |
           atrasada == 1 |
           sem_end == 1) %>%
  mutate(nao_iniciada = ifelse(nao_iniciada == 1, "11", "NA"),
         paralisada_nao_off = ifelse(  paralisada_nao_off == 1, "10" , "NA"),
         paralisada_off = ifelse(paralisada_off == 1, "9", "NA"),
         atrasada = ifelse(atrasada == 1, "7", "NA"),
         sem_end = ifelse(sem_end == 1, "8", "NA"),  
         id_da_inc = paste(nao_iniciada, paralisada_nao_off, paralisada_off, atrasada, sem_end, sep=";" ),
         id_da_inc = gsub("NA;", "", id_da_inc),
         id_da_inc = gsub(";NA", "", id_da_inc),
         'Usuário' = "TDP2018",
         'Comentário' = NA) %>%
  select('Usuário', id, nome, 'Comentário', id_da_inc) %>%
  rename('ID da OBRA' = id,
         'Nome da Obra' = nome,
         'ID das Incongruências' = id_da_inc)

# salvando:
# save(envio_acao, file="envio_acao_v2.Rdata")
# write.csv(envio_acao, file="envio_acao_v2.csv", row.names = FALSE, fileEncoding = "UTF-8")

ids_envio_acao <- envio_acao$`ID da OBRA`  

#Planilhas para imprensa:

obras_campanha <- sit_obras_final %>%
  filter(id %in% ids_envio_acao) %>%
  mutate(paralisada = as.numeric(paralisada),
         atrasada = as.numeric(atrasada),
         sem_end = as.numeric(sem_end),
         nao_iniciada = as.numeric(nao_iniciada),
         situacao_segundo_tbrasil = ifelse(sem_end == 1, 
                                           ifelse( paralisada + nao_iniciada + atrasada > 0 ,
                                                   paste(situacao_segundo_tbrasil, "e sem endereço", sep=" "), 
                                                   "sem endereço"), 
                                           situacao_segundo_tbrasil)) %>%
  select(id, nome, responsabilidade, municipio, uf, logradouro, termo_convenio, status,
         data_final_prevista_e_estimada,tipo_data_final,
         pagto_final, valor_estimado, pactuado_cte_nov2018, situacao_segundo_tbrasil) %>%
  rename(data_de_entrega = data_final_prevista_e_estimada,
         pagamento_total = pagto_final,
         valor_pactuado = pactuado_cte_nov2018,
         problema_encontrado = situacao_segundo_tbrasil) %>%
  mutate(problema_encontrado = gsub(" e ", " / ", problema_encontrado),
         tipo_data_final = ifelse(is.na(data_de_entrega), "Não foi possível estimar", tipo_data_final)) 


### Anexos:

#Informações compiladas de cada unidade federativa
anexo1 <- sit_obras_final %>%
  filter(id %in% ids_envio_acao) %>%
  group_by(uf, responsabilidade) %>%
  summarise(obras = n(),
            valor_repassado = sum(pagto_final, na.rm=TRUE)) %>%
  spread(responsabilidade, obras) %>%
  clean_names() %>%
  mutate(num = ifelse(!is.na(municipal), "valor_municipais", "valor_estaduais")) %>%
  spread(num, valor_repassado) %>%
  ungroup() %>%
  group_by(uf) %>%
  summarise(obras_municipais_notificadas = sum(municipal, na.rm=TRUE),
            repasse_estimado_obras_municipais = sum(valor_municipais, na.rm=TRUE),
            obras_estaduais_notificadas = sum(estadual, na.rm=TRUE),
            repasse_estimado_obras_estaduais = sum(valor_estaduais, na.rm=TRUE)) %>%
  mutate(obras_municipais_notificadas = ifelse(is.na(obras_municipais_notificadas), 0, obras_municipais_notificadas),
         obras_estaduais_notificadas = ifelse(is.na(obras_estaduais_notificadas), 0, obras_estaduais_notificadas), 
         total_repasses_estimados = repasse_estimado_obras_municipais + repasse_estimado_obras_estaduais,
         repasse_estimado_obras_municipais	= ifelse(repasse_estimado_obras_municipais == 0, NA, round(repasse_estimado_obras_municipais)),
         repasse_estimado_obras_estaduais = ifelse(repasse_estimado_obras_estaduais == 0, NA, round(repasse_estimado_obras_estaduais)),
         total_obras_notificadas = obras_estaduais_notificadas + obras_municipais_notificadas) %>%
  select(1:5, 7,6)


save(anexo1, file="anexo1.Rdata")
# write.csv(anexo1, file="anexo1.csv", dec = ",", sep=";", row.names = FALSE)

#subindo no drive:
# anexo1_sheet <- drive_upload(
#   "anexo1.csv",
#   path="",
#   name = "Anexo1",
#   type = "spreadsheet")

#Anexo 2 Obras de responsabilidade municipal notificadas

anexo2 <- sit_obras_final %>%
  filter(id %in% ids_envio_acao,
         responsabilidade == "Municipal") %>%
  group_by(municipio, uf) %>%
  summarise(quantidade_de_obras_notificadas = n(),
            total_repasse_estimado_obras_notificadas = round(sum(pagto_final, na.rm=TRUE))) %>%
  mutate(total_repasse_estimado_obras_notificadas = ifelse(total_repasse_estimado_obras_notificadas == 0, NA,
                                                           total_repasse_estimado_obras_notificadas))

save(anexo2, file="anexo2.Rdata")
#write.csv(anexo2, file="anexo2.csv", dec = ",", sep=";", row.names = FALSE)

# anexo2_sheet <- drive_upload(
#   "anexo2.csv",
#   path="",
#   name = "Anexo2",
#   type = "spreadsheet")

anexo3 <- obras_campanha %>%
  mutate(pagamento_total = ifelse(is.na(pagamento_total), 0, pagamento_total)) 

save(anexo3, file="anexo3.Rdata")
# write.csv(anexo3, file="anexo3.csv", dec = ",", sep=";", row.names = FALSE)

# anexo3_sheet <- drive_upload(
#   "anexo3.csv",
#   path="",
#   name = "Anexo3",
#   type = "spreadsheet")

################################ Números que aparecem no relatório #################################################

# As tabelas (T) foram exportadas para excel e usadas no arquivo do relatório
# Os gráficos (G) foram também exportados para o excel. 

#G1:
#Obras a ser entregues (pendentes) e que serão e não notificadas

sit_obras_final %>%
  filter(cancelada == 0) %>%
  filter(concluida == 0) %>%
  mutate(problema = ifelse(id %in% ids_envio_acao, 1, 0)) %>%
  group_by(problema) %>%
  summarise(obras =n())

#G2 Status das obras pendentes:

sit_obras_final %>%
  group_by(status) %>%
  summarise(obras = n())

#G3 convênios firmados por ano

sit_obras_final %>%
  group_by(ano_convenio) %>%
  summarise(obras = n())

#G4 obras que deveriam ter sido entregues por ano.

sit_obras_final %>%
  filter(!status %in% c("concluida", "cancelada")) %>%
  group_by(ano_data_final_prevista_e_estimada) %>%
  summarise(obras = n()) 
 

#T1
sit_obras_final %>%
  mutate(status = ifelse(status %in% c("não iniciada", "paralisada", "execucao"), 
                         "pendente", status )) %>%
  group_by(ano_data_final_prevista_e_estimada, status) %>%
  summarise(obras = n()) %>%
  spread(status, obras) %>%
  rename('Ano previsto para conclusão da obra' = ano_data_final_prevista_e_estimada)

#T2

t2_1 <- obras_campanha %>%
  filter(responsabilidade == "Estadual") %>%
  mutate(pagamento_total = as.numeric(pagamento_total)) %>%
  group_by(uf) %>%
  summarise(quantidade_obras_estaduais_notificadas = n(),
            valor_pago_obras_estaduais_notificadas = sum(pagamento_total))

t2_2 <- obras_campanha %>%
  filter(responsabilidade == "Municipal") %>%
  mutate(pagamento_total = as.numeric(pagamento_total)) %>%
  group_by(uf) %>%
  summarise(quantidade_obras_municipais_notificadas = n(),
            valor_pago_obras_municipais_notificadas = sum(pagamento_total))

t2 <- t2_1 %>%
  left_join(t2_2, by="uf")

#entes que receberão alertas
#prefeituras
nrow(municipios_notificados)
nrow(ufs_notificadas)

obras_campanha %>%
  group_by(responsabilidade) %>%
  summarise(perc = n()/3233)

sit_obras_final %>%
  group_by(situacao_segundo_tbrasil) %>%
  summarise(obras = n(),
            perc = n()/14466)

sit_obras_final %>%
  filter(!status %in% c("concluida", "cancelada")) %>%
  mutate(atrasada = as.numeric(atrasada)) %>%
  summarise(obras = sum(atrasada)/5466)

#canceladas em agosto:
load("obras04092018.Rdata")
obras_setembro <- obras

obras_setembro %>%
  clean_names() %>%
  filter(!id %in% id_controle_campanha_final,   #tirando os ids que estão no controle da campanha
         tipo_do_projeto %notin% not_project) %>%
  group_by(situacao) %>%
  summarise(obras = n())

sit_obras_final %>%
  group_by(status) %>%
  summarise(obras = n(),
            perc = n()/14466)

convenio_mais_de_uma_obra <- sit_obras_final %>%
  group_by(termo_convenio) %>%
  mutate(qtde_obras_convenio = n()) %>%
  filter(qtde_obras_convenio > 1)

ids_conv_mais_de_uma_bra <- convenio_mais_de_uma_obra$id

obras_campanha %>%
  filter(id %in% ids_conv_mais_de_uma_bra) %>%
  nrow()

#T3
sit_obras_final %>%
  filter(id %in% ids_envio_acao) %>%
  group_by(uf, responsabilidade) %>%
  summarise(obras = n(),
            valor_repassado = sum(pagto_final, na.rm=TRUE)) %>%
  spread(responsabilidade, obras) %>%
  clean_names() %>%
  mutate(num = ifelse(!is.na(municipal), "valor_municipais", "valor_estaduais")) %>%
  spread(num, valor_repassado) %>%
  ungroup() %>%
  group_by(uf) %>%
  summarise(obras_municipais_notificadas = sum(municipal, na.rm=TRUE),
            repasses_municipais_estimado = sum(valor_municipais, na.rm=TRUE),
            obras_estaduais_notificadas = sum(estadual, na.rm=TRUE),
            repasses_estaduais_estimado = sum(valor_estaduais, na.rm=TRUE)) %>%
  mutate(repasse_estimado_obras_municipais	= ifelse(repasses_municipais_estimado == 0, NA, round(repasses_municipais_estimado)),
         repasse_estimado_obras_estaduais = ifelse(repasses_estaduais_estimado == 0, NA, round(repasses_estaduais_estimado)))

## T4
obras_campanha %>%
  filter(id %in% ids_envio_acao) %>%
  mutate(pagamento_total = as.numeric(pagamento_total)) %>%
  group_by(status, problema_encontrado) %>%
  summarise(obras = n(),
            valor_repassado_estimado = sum(pagamento_total, na.rm=T))