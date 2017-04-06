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
setwd("D:/Users/B31099033/Documents/Mapa/raw-data")

# setClass("myDate")
# setAs("character", "myDate", function(from) as.Date(from, format="%d/%m/%Y"))

# Open original dataset
oscip <- read.csv2("tb_oscip_10102016.csv",
                   quote = "", 
                   stringsAsFactors = F,
                   encoding = "latin",
                   na.strings = "",
                   col.names = c("id_estab", "nm_razao_social",
                                     "nm_logradouro", "num_cep", 
                                     "nm_cidade", "sigla_uf",
                                     "num_telefone", "NULL",
                                     "dt_emissao", "tx_finalidade"),
                   colClasses = c(rep("character", 7), "NULL",
                                      "character", "character"))

## Open and manipulate desqualified OSCIPs
oscip_desq1 <- read_excel("tb_oscip_cancelada_07062016.xlsx", col_names = c("nrow", "razao_social", "id_estab", "dt_cancelado"), skip = 2)
oscip_desq2 <- read_excel("tb_oscip_cancelada_20082016.xlsx", col_names = c("nrow", "razao_social", "id_estab", "dt_cancelado"), skip = 1)
oscip_desq3 <- read_excel("tb_oscip_cancelada_29122016.xlsx", col_names = c("nrow", "razao_social", "id_estab", "dt_cancelado"), skip = 1)

clean_id <- function(x){
    x <- str_replace_all(x, "\\.", "") 
    x <- str_replace_all(x, "/", "")
    x <- str_replace_all(x, "-", "")
    x <- str_pad(x, 14, "left", "0")
    return(x)
}

oscip_desq1 <- oscip_desq1 %>% mutate(id_estab = clean_id(id_estab)) %>%  select(id_estab)
oscip_desq2 <- oscip_desq2 %>% mutate(id_estab = clean_id(id_estab)) %>% select(id_estab)
oscip_desq3 <- oscip_desq3 %>% mutate(id_estab = clean_id(id_estab)) %>% select(id_estab)
oscip_desq <- rbind(oscip_desq1, oscip_desq2, oscip_desq3)
rm(oscip_desq1, oscip_desq2, oscip_desq3, clean_id)

## Clean and save data =====================================================
setwd("..")

#' The commands below should save two versions of the dataset: 
#' 1) Map of OSCs (ID and issue date); and 
#' 2) Reports (ID, company name, city, state, issue date, and objective).
#' 
#' We begin with the second.

oscip %>%
    tbl_df %>%
    mutate_each(funs(str_trim(., "both")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "\"", "")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "\'", "")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "0\\*\\*", "")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "xx", "")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "\\*", "")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "-\\*\\*", "")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "XX", "")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "\\[", "\\(")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., "\\]", "\\)")), id_estab:dt_emissao) %>%
    mutate_each(funs(str_replace_all(., ";", "")), id_estab:dt_emissao) %>%
    mutate_each(funs(ifelse(.=="", NA, .))) %>%
    mutate_each(funs(ifelse(is.na(.), "", .))) %>%
    mutate(id_estab = str_pad(id_estab, 14, pad = 0, "left"),
           nm_razao_social = str_trim(nm_razao_social, "both"),
           nm_razao_social = str_replace_all(nm_razao_social, pattern = "^-", ""),
           nm_razao_social = str_to_title(nm_razao_social),
           nm_razao_social = str_trim(nm_razao_social, "both"),
           num_telefone = str_replace_all(num_telefone, "\\( ", "\\("),
           num_telefone = str_replace_all(num_telefone, " \\)", "\\)"),
           num_telefone = str_replace_all(num_telefone, "x", ""),
           num_telefone = str_replace_all(num_telefone, "\\*\\*", ""),
           num_telefone = str_sub(num_telefone, 1, 15),
           num_telefone = str_replace_all(num_telefone, "\\(0", "\\("),
           num_telefone = str_replace_all(num_telefone, "/$", ""),
           num_telefone = str_trim(num_telefone, "both")) %>% 
    anti_join(., oscip_desq) %>% 
    select(id_estab, nm_razao_social, nm_cidade, sigla_uf, dt_emissao, # select variables for report
           tx_finalidade) %>% 
    write.table(.,
            file = "./data/tb_oscip.csv",
            quote = F,
            row.names = F, 
            sep = ";")
