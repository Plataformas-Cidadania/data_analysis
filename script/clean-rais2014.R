## Load packages ===============================================================
if (!require("install.load")) install.packages("install.load")
install.load::install_load("readxl", "tidyverse", "stringr", "plyr", "sqldf", 
                           "tcltk", "data.table", "reshape2", "maptools", 
                           "RColorBrewer", "bit64", "rgdal", "ggmap", "janitor",
                           "ggrepel", "gridExtra")

options(dplyr.width = Inf)
# options(dplyr.print_max = 2000)
options(max.print = 1000, scipen = 999, width = 90)

## Load data ===============================================================
estb <- file("estb.txt")

estab14 <- sqldf("select * 
                 from estb 
                 where `Natureza.Jurídica` > 3000 AND 
                 `Natureza.Jurídica` < 4000", 
                 file.format = list(header = T, sep = ";"))
saveRDS(estab14, file = "estab14_esfl.rds")
rm(estb)

cd_ibge <- read.delim("AR_BR_RG_UF_MUN_2015.txt",
                      col.names = c("NULL", "cd_uf", "nm_uf", "nm_uf_sigla",
                                    "cd_mun", "nm_mun", "NULL"),
                      colClasses = c("NULL", rep("character", 5), "NULL"),
                      encoding = "ASCII")

cd_ibge <- mutate(cd_ibge, cd_mun = as.character(str_sub(cd_mun, 1, 6)),
                  nm_uf = stringi::stri_encode(nm_uf, "", "UTF-8"),
                  nm_mun = stringi::stri_encode(nm_mun, "", "UTF-8"))

## Clean column names and select variables =================================
estab14 <- estab14 %>%
    setNames(tolower(names(.))) %>%
    setNames(str_replace_all(names(.), "\\.\\.\\.", "\\.")) %>%
    setNames(str_replace_all(names(.), "\\.\\.", "\\.")) %>%
    setNames(str_replace_all(names(.), "\\.", "_")) %>%
    setNames(str_replace_all(names(.), "_$", "")) %>%
    setNames(iconv(names(.), from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
    mutate(cnpj_cei = str_pad(cnpj_cei, 14, side = "left", pad = "0"),
           razao_social = str_trim(razao_social, side = "both")) %>%
    select(cnpj_cei, razao_social, natureza_juridica, nome_bairro:uf)

## Load function for identifying OSCs ======================================
source("./lib/find-osc.R", encoding = "UTF-8")
estab14 <- limpaRais(estab14)

## Tranform data (some more cleaning and processing) ======================

estab14 <- estab14 %>%
    mutate(razao_social = str_to_title(razao_social),
           nome_logradouro = str_trim(nome_logradouro, side = "both"),
           nome_bairro = str_trim(nome_bairro, side = "both"),
           razao_social = str_replace(razao_social, "^\\.", ""),
           razao_social = str_replace(razao_social, "^/", ""),
           razao_social = str_replace(razao_social, "^,", ""),
           razao_social = str_replace(razao_social, "^\\(Uex\\)", "Uex"),
           razao_social = str_replace(razao_social, "^\\(Daee\\)", "Daee"),
           razao_social = str_replace(razao_social, "^\\(Aec\\)", "Aec"),
           razao_social = str_replace(razao_social, "^\\(Emef\\)", "Emef"),
           razao_social = str_replace(razao_social, "^\\(08\\)", "08"),
           cnae_2_0_classe = str_pad(cnae_2_0_classe, width = 5, side = "left", pad = "0"),
           cnae_2_0_subclasse = str_pad(cnae_2_0_subclasse, width = 7, side = "left", pad = "0"),
           divisao_cnae20 = as.character(str_sub(clas_cnae20, 1, 2))) %>%
    
estab14 <- estab14 %>% 
    select(id_estab:regiao, divisao_cnae20, clas_cnae20:ind_osc)

estab14 <- estab14 %>%
    tbl_df %>%
    mutate(regiao = ifelse(uf > 30 & uf < 36, 3,
                                  ifelse(uf > 40 & uf < 44 , 4,
                                         ifelse(uf > 10 & uf < 18 , 1,
                                                ifelse(uf > 20 & uf < 30, 2,
                                                       ifelse(uf > 49 & uf < 54,
                                                              5, NA))))),
                  tamestab2 = ifelse(qtd_vinculos_ativos == 0, 
                                     1, 
                                     ifelse(qtd_vinculos_ativos > 0 & qtd_vinculos_ativos < 5, 
                                            2, 
                                            ifelse(qtd_vinculos_ativos > 4 & qtd_vinculos_ativos < 20, 
                                                   3, 
                                                   ifelse(qtd_vinculos_ativos > 19 & qtd_vinculos_ativos < 100, 
                                                          4, 
                                                          ifelse(qtd_vinculos_ativos >= 100, 
                                                                 5, 
                                                                 NA)))))) %>%
    select(id_estab = cnpj_cei, codemun = municipio, uf, regiao, divisao_cnae20,
           clas_cnae20 = cnae_2_0_classe, sbcl_cnae20 = cnae_2_0_subclasse,
           nat_jur2014 = natureza_juridica, razao_social, 
           tamestab = tamanho_estabelecimento, tamestab2, qtd_vinc_ativos = qtd_vinculos_ativos,
           nome_logradouro, num_logradouro = numero_logradouro,
           nome_bairro, cep_estab, email_estab = email_estabelecimento,
           data_abertura, data_encerramento, 
           num_telefone = numero_telefone_empresa, ind_osc) %>%
    mutate(email_estab = str_trim(email_estab, "both"),
           email_estab = str_to_lower(email_estab))

estab14 <- estab14 %>% 
    mutate(razao_social = str_replace(razao_social, "\\.$", ""),
           razao_social = str_replace(razao_social, "-$", ""),
           razao_social = str_replace(razao_social, "\\($", ""),
           razao_social = str_replace(razao_social, ",$", ""),
           razao_social = str_replace(razao_social, "/$", ""),
           razao_social = str_trim(razao_social, "both"))

estab14 <- estab14 %>%
    dplyr::mutate(data_abertura = ifelse(data_abertura!=0,
                                         str_pad(data_abertura, width = 8, side = "left", pad = 0),
                                         data_abertura),
                  data_encerramento = ifelse(data_encerramento!=0,
                                             str_pad(data_encerramento, width = 8, side = "left", pad = 0),
                                             data_encerramento),
                  data_abertura = ifelse(data_abertura!=0,
                                         paste(str_sub(data_abertura, 1, 2),
                                        str_sub(data_abertura, 3, 4),
                                        str_sub(data_abertura, 5, 8),
                                        sep = "/"), data_abertura),
                  data_encerramento = ifelse(data_encerramento!=0,
                                             paste(str_sub(data_encerramento, 1, 2),
                                        str_sub(data_encerramento, 3, 4),
                                        str_sub(data_encerramento, 5, 8),
                                        sep = "/"), data_encerramento))

estab14 <- estab14 %>%
    dplyr::mutate(data_abertura = ifelse(data_abertura==0,
                                         "",
                                         data_abertura),
                  data_encerramento = ifelse(data_encerramento==0,
                                             "",
                                             data_encerramento))

estab14 <- estab14 %>%
    mutate(codemun = as.character(codemun)) %>%
    left_join(., cd_ibge[, c("nm_uf", "cd_mun", "nm_mun")], 
              by = c("codemun" = "cd_mun")) %>%
    mutate(nm_uf = str_to_upper(iconv(nm_uf, from = "UTF-8", to = "ASCII//TRANSLIT")),
           nm_mun = str_to_upper(iconv(nm_mun, from = "UTF-8", to = "ASCII//TRANSLIT")))

estab14 <- estab14 %>%
    select(id_estab, codemun, nm_mun, uf, nm_uf, regiao:ind_osc)

## Save data ===============================================================
setwd("C:/Users/Andre/Documents")
setwd("./Projetos/2015/Ipea/Projetos/Mapa/Bases/Perfil/RAIS/Dados/2014/2-Analitico/")
save(estab14, file = "estab14_analitico.Rda")
write.table(estab14,  
            file = "tb_rais.csv", 
            row.names = F, 
            quote = F,
            sep = ";")