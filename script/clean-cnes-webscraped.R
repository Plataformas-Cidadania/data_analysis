## Load packages ===============================================================
if (!require("install.load")) install.packages("install.load")
install.load::install_load("readxl", "tidyverse", "stringr", "plyr",
                           "tcltk", "data.table", "reshape2", "survey",
                           "psych", "ffbase2", "ffbase", "janitor")

options(max.print = 1000, scipen = 999, width = 90, digits = 3)
options(dplyr.width = Inf)

## Load data ===================================================================
load("./raw-data/links_full.Rda")
sapply(dir("./raw-data", pattern = "Rda")[-8], load, .GlobalEnv)

oscip <- read.delim("./data/tb_oscip_052016.txt", colClasses = c("character", rep("NULL", 21)))
oscip_cancelada <- read.delim("./raw-data/tb_oscip_cancelada.txt", 
                              skip = 1, 
                              colClasses = c(rep("NULL", 2), "character", "NULL"))
upf <- read.delim("./data/tb_entidade_CNES_2015.txt", 
                  colClasses = c(rep("character", 2), rep("NULL", 6)))
setwd("./data")

## Clean the data ==============================================================

#' assets.all OK OK
#' boards.all OK OK
#' sources.all OK OK
#' directors.all OK OK
#' contracts.all OK OK
#' contracts.details.all OK OK 
#' budget.all OK
#' links OK OK

assets.all %>%
    as.data.frame(.) %>%
    setNames(tolower(names(.))) %>%
    setNames(iconv(names(.), from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
    setNames(str_replace_all(names(.), "\\(s\\)|\\(-\\)", "")) %>%
    setNames(str_replace_all(names(.), "de |a |e | dos|valores| -|csll|cp|\\(saldo\\)|\\(fundo patrimonial\\)", "")) %>%
    setNames(str_replace_all(names(.), ",", "")) %>%
    setNames(str_replace_all(names(.), "/", " ")) %>%
    setNames(str_trim(names(.), side = "both")) %>%
    setNames(str_replace_all(names(.), " ", "_")) %>%
    setNames(paste0("vl_", names(.))) %>%
    dplyr::rename(cnpj = vl_cnpj, ano = vl_year, url_principal = vl_main_url,
                  vl_provisao_devedores = vl_provisao_pardevedores_duvidosos,
                  vl_provisao_para_irend = vl_provisao_parirend) %>%
    write.table(., "tb_balanco.csv", sep = ";", quote = F, row.names = F)

boards.all %>%
    dplyr::rename(ano = year, nm_diretor = name, nm_cargo = position, 
                  in_empregado_publico = public_employee) %>%
    write.table(., "tb_diretoria.csv", sep = "\t", quote = F, row.names = F)

sources.all %>%
    as.data.frame(.) %>%
    setNames(c("cnpj", "ano", "url_principal", 
               "propria_servico", "propria_doacao_mensalidade", 
               "privada_doacao_parceria", "privada_doacao_even", 
               "publica_parceria", "internacional_privada", 
               "internacional_publica")) %>%
    write.table(., "tb_recursos.csv", sep = ";", quote = F, row.names = F)

directors.all %>%
    as.data.frame(.) %>%
    setNames(c("cnpj", "ano", "url_principal", "tp_sede", "sigla_uf", 
               "nm_municipio", "nm_cartorio", "dt_reg", "in_mudanca_previa",
               "dt_inicio_mandato_atual", "dt_fim_mandato_atual",
               "nm_diretor", "nm_ocupacao", "nm_cargo", "nm_genero",
               "in_empregado_publico", "in_funcao_remunerada", 
               "nm_funcao_remunerada")) %>%
    write.table(., "tb_diretores.csv", sep = "\t", quote = F, row.names = F)

contracts.all %>%
    setNames(c("cnpj", "ano", "num_parceria", "nm_orgao_entidade_parceira", 
               "tp_instrumento_parceria", "tp_ente_federado")) %>%
    write.table(., "tb_parcerias.csv", sep = "\t", quote = F, row.names = F)

contracts.details.all %>%
    as.data.frame(.) %>%
    setNames(c("cnpj", "ano", "num_parceria", "nm_orgao_entidade_parceira", 
               "nm_posicao_estrutura_adm", "tp_ente_federado",
               "tp_fonte_recursos", "tp_instrumento_parceria",
               "dt_publicacao", "vl_recursos_previstos", 
               "vl_recursos_repassados", "num_beneficiarios",
               "dt_inicio_atividades", "dt_fim_atividades",
               "tx_objetivo_parceria")) %>%
    mutate(tx_objetivo_parceria = str_replace_all(tx_objetivo_parceria, "\n", "")) %>%
    write.table(., "tb_parcerias_detalhes.csv", sep = "\t", quote = F, 
                row.names = F)

budget.all %>%
    as.data.frame(.) %>%
    setNames(tolower(names(.))) %>%
    setNames(str_replace_all(names(.), "\\(exceto saãºde/educaã§ã£o\\)|\\(-\\)", "")) %>%
    setNames(str_replace_all(names(.), "de |a |e | dos| -|csll|cp|sobre |no | s/|com |em |ou |para ", "")) %>%
    setNames(str_trim(names(.), side = "both")) %>%
    setNames(str_replace_all(names(.), " ", "_")) %>%
    setNames(str_replace_all(names(.), ",", "")) %>%
    setNames(str_replace_all(names(.), "\\.", "")) %>%
    setNames(str_replace_all(names(.), "ã§ãµ", "co")) %>%
    setNames(str_replace_all(names(.), "ãª", "e")) %>%
    setNames(str_replace_all(names(.), "ã§", "c")) %>%
    setNames(str_replace_all(names(.), "ãº", "u")) %>%
    setNames(str_replace_all(names(.), "ã£", "a")) %>%
    setNames(str_replace_all(names(.), "ã¡", "a")) %>%
    setNames(str_replace_all(names(.), "ã©", "e")) %>%
    setNames(str_replace_all(names(.), "ã³", "o")) %>%
    setNames(str_replace_all(names(.), "ã", "i")) %>%
    setNames(str_replace_all(names(.), "i\u0081", "a")) %>%
    setNames(str_replace_all(names(.), "-", "_")) %>%
    setNames(paste0("vl_", names(.))) %>%
    dplyr::rename(cnpj = vl_cnpj, ano = vl_year, 
                  url_principal = vl_main_url, 
                  vl_receita_convenios_saude_privados = vl_receitconvenios_sauprivados,
                  vl_receita_convenios_saude_conveniados = vl_receitconvenios_sauprivados,
                  vl_prest_servicos_saude_conveniados = vl_prest_servicos_saunao_conveniados,
                  vl_sus_sistema_unico_saude = vl_sus_sistemišnico_saude) %>%
    write.table(., "tb_resultado_exercicio.csv", sep = "\t", quote = F, 
                row.names = F)

links %>%
    setNames(c("cnpj", "ano", "url_relatorio", "num_relatorio")) %>%
    write.table(., "tb_links_relatorios.csv", sep = "\t", quote = F, row.names = F)

qualif <- links %>% 
    dplyr::select(cnpj) %>% 
    mutate(cnpj = str_replace_all(cnpj, "\\.", ""),
           cnpj = str_replace_all(cnpj, "/", ""),
           cnpj = str_replace_all(cnpj, "-", "")) %>%
    filter(cnpj!="", !duplicated(cnpj))

oscip_cancelada <- oscip_cancelada %>%
    setNames(tolower(names(.))) %>%
    mutate(cnpj = str_replace_all(cnpj, "\\.", ""),
           cnpj = str_replace_all(cnpj, "/", ""),
           cnpj = str_replace_all(cnpj, "-", ""))

oscip <- oscip %>%
    setNames(tolower(names(.))) %>%
    filter(!cnpj %in% oscip_cancelada$cnpj, !duplicated(cnpj)) %>%
    mutate(cd_qualificacao = 1, des_qualificacao = "OSCIP")

upf <- upf %>%
    setNames(tolower(names(.))) %>%
    mutate(cnpj = str_replace_all(cnpj, "\\.", ""),
           cnpj = str_replace_all(cnpj, "/", ""),
           cnpj = str_replace_all(cnpj, "-", ""),
           cd_qualificacao = 2, des_qualificacao = "UPF") %>%
    filter(título == "DIVOT", !duplicated(cnpj)) %>%
    dplyr::select(cnpj, cd_qualificacao, des_qualificacao)

## Combine datasets from CNES Publico and OSCIPs ===============================
qualif %<>% left_join(qualif, oscip)
rm(oscip, oscip_cancelada)

## Combine datasets from CNES publico and UPFs =================================
qualif <- qualif %>%
    left_join(., upf, by = "cnpj") %>%
    transmute(cnpj, 
              cd_qualificacao = ifelse(is.na(cd_qualificacao.x), 
                                       cd_qualificacao.y,
                                       cd_qualificacao.x),
              des_qualificacao = ifelse(is.na(des_qualificacao.x), 
                                        des_qualificacao.y,
                                        des_qualificacao.x))

write.table(qualif, "tb_qualificacoes.csv", sep = "\t", quote = F, row.names = F)
save(qualif, file = "qualif.Rda")
rm(list=ls())