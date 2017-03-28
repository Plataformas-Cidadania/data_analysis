cleanRais <- function(x){
    
    if (!require("install.load")) install.packages("install.load")
    install.load::install_load("tidyverse", "stringr")
    
    ## Creates binary variable "ind_osc" =======================================
    x$ind_osc <- NA
    
    ## RAIS 2009 ===============================================================
    if(nrow(x)==661601){
        x$ind_osc <- ifelse(x$NAT.JURIDICA=="3042" | 
                                x$NAT.JURIDICA=="3050" | 
                                x$NAT.JURIDICA=="3069" |
                                x$NAT.JURIDICA=="3220" |
                                x$NAT.JURIDICA=="3999", 1, 0)
        
        x$ind_osc[is.na(x$IND.RAIS.NEG) & x$IND.ATIV.ANO=="0" & x$ESTOQUE=="0"] <- 0
        
        }
    
    ## RAIS 2010 ===============================================================
    if(nrow(x)==669352){
        x$ind_osc <- ifelse(x$NAT.JURIDICA=="3069" | 
                                    x$NAT.JURIDICA=="3220" | 
                                    x$NAT.JURIDICA=="3999", 1, 0)
        
        x$ind_osc[is.na(x$IND.RAIS.NEG) & x$IND.ATIV.ANO=="0" & x$ESTOQUE=="0"] <- 0
    }
    
    ## RAIS 2011 ===============================================================
    else if(nrow(x)==694688){
        x$ind_osc <- ifelse(x$nat_jur2009=="3069" |
                                x$nat_jur2009=="3042" | 
                                x$nat_jur2009=="3050" | 
                                x$nat_jur2009=="3220" | 
                                x$nat_jur2009=="3999", 1, 0)
        
        x$ind_osc[is.na(x$ind_rais_neg) & x$ind_ativ_ano=="0" & x$qtd_vinc_ativos==0] <- 0
    }
    
    
    ## RAIS 2012 ===============================================================
    else if(nrow(x)==709607){
        x$ind_osc <- ifelse(x$Natureza.Jurídica=="3042" | 
                                x$Natureza.Jurídica=="3069" | 
                                x$Natureza.Jurídica=="3220" | 
                                x$Natureza.Jurídica=="3999", 1, 0)
        
        x$ind_osc[is.na(x$Ind.Rais.Negativa) & x$Ind.Atividade.Ano=="0" & x$Qtd.Vínculos.Ativos==0] <- 0
    }
    
    ## RAIS 2013 ===============================================================
    else if(nrow(x)==712806){
        x$ind_osc <- ifelse(x$nat_jur2009=="3069" | 
                                      x$nat_jur2009=="3220" | 
                                      x$nat_jur2009=="3999", 1, 0)
        
        x$ind_osc[is.na(x$ind_rais_neg) & x$ind_ativ_ano=="0" & x$qtd_vinc_ativos==0] <- 0
    }
    
    ## RAIS 2014 ===============================================================
    else if(nrow(x)==716641){
        x$ind_osc <- ifelse(x$natureza_juridica==3069 | 
                                x$natureza_juridica==3220 | 
                                x$natureza_juridica==3301 |
                                x$natureza_juridica==3999, 1, 0)
        
    # x$ind_osc[x$ind_encerr_ativ=="1" & x$qt_vinc_ativ_dez==0] <- 0
    x$ind_osc[is.na(x$ind_rais_negativa) & x$ind_atividade_ano==0 & x$qtd_vinculos_ativos==0] <- 0
    }
    
    ## Remove natural persons (pesoas físicas) =================================
    if(nrow(x)==694688 | nrow(x)==712806 | nrow(x)==716641){
        x$ind_osc[str_detect(x$tipo_estab, "3") &
                                !str_detect(x$razao_social, "^CONGREGACAO|^SITIO|^ASSOC|^SOCIEDADE|IGREJA|^COMUNIDADE|^CHACARA|^CAIXA|^CAMARA|^PAROQUIA|^ARQUIDIOCESE|^ORDEM|^FAZ|^FRATERNIDADE|DIOCESE|^OBRA|^LAR|^MINISTERIO|ESTANCIA|UNIAO|MITRA DIOC|PARCERIA|IDN|EDIFICIO|ESPOLIO|AGRO |CONTABILIDADE")] <- 0
    
    }
    
    else if(nrow(x)==709607){
        
        x$ind_osc[str_detect(x$Tipo.Estab, "3") &
                                  !str_detect(x$razao_social, "^CONGREGACAO|^SITIO|^ASSOC|^SOCIEDADE|IGREJA|^COMUNIDADE|^CHACARA|^CAIXA|^CAMARA|^PAROQUIA|^ARQUIDIOCESE|^ORDEM|^FAZ|^FRATERNIDADE|DIOCESE|^OBRA|^LAR|^MINISTERIO|ESTANCIA|UNIAO|MITRA DIOC|PARCERIA|IDN|EDIFICIO|ESPOLIO|AGRO |CONTABILIDADE")] <- 0
        
    }
    
    else if(nrow(x)==661601 | nrow(x)==669352){
        
        x$ind_osc[str_detect(x$TIPO.ESTBL, "3") &
                                  !str_detect(x$razao_social, "^CONGREGACAO|^SITIO|^ASSOC|^SOCIEDADE|IGREJA|^COMUNIDADE|^CHACARA|^CAIXA|^CAMARA|^PAROQUIA|^ARQUIDIOCESE|^ORDEM|^FAZ|^FRATERNIDADE|DIOCESE|^OBRA|^LAR|^MINISTERIO|ESTANCIA|UNIAO|MITRA DIOC|PARCERIA|IDN|EDIFICIO|ESPOLIO|AGRO |CONTABILIDADE")] <- 0
        
    }
    
    ## Remove school financial boards (caixas escolares) and the like ==========
    x$ind_osc[str_detect(x$razao_social, "^CX E|CX\\.E|CX\\. E|^CX\\. E\\.|^CXA E|^CXA\\.E|^CAI E|^CAIX E|^CAIXA ESC|^CAIXA E |^CAIA E |^CAIXA E\\.|^CA ES|^CA\\. ES|^C ESC DA|^C ESC DO|^C ESCOLAR DA|^C\\. ESCOLAR DA|CAIXA ES ")] <- 0
    
    x$ind_osc[(str_detect(x$razao_social, "CX ESC") & 
                  !str_detect(x$razao_social, "PAIS")) & 
        !str_detect(x$razao_social, "CONS")] <- 0
    
    ## Remove political parties ================================================
    
    x$ind_osc[str_detect(x$razao_social, "^PARTIDO") & 
                        !str_detect(x$razao_social, "^PARTIDO DO MERITO")] <- 0
    
    x$ind_osc[(str_detect(x$razao_social, "DIR MUN") &
                  !str_detect(x$razao_social, "UNDIME")) & 
        !str_detect(x$razao_social, "PGU")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "DIR\\. MUN|DIR\\.MUN|DIRETORIO MUN |DIRETORIO MUN\\.|DIRETORIO M |DIRETORIO M\\.|DIRETORIO DO|DIRETRIO MUNICIPAL|DIRETORIO MUNICIPAL|DIRETORIO DA COMISSAO|DIRETORIO MUNIC\\.|DIRETORIO MINIC\\.|DIRETORIO MUNIC |DIRETORIO PARTIDO|DIRET MUN|DIRET\\. MUN|DIRET\\.DO|DIRET\\. DO|DIRETORIA MUN|DIR\\. MUN\\.|DIR\\.MUN\\.|DIR DO PARTIDO|COMISSAO EXECUTIVA|^COM EXEC|COMIS E|COMISSAO EXEC |COM PROVISORIA|COM\\. PROV|COM PROV|COMISSAO PROV|COMISAO PROV|C P M DO DEM|C P M DO PSB|COM EXER|DIRECAO MUN|DIRETORIO MINICIPAL")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "COMISSAO MUNIC PROVI|COMISSAO MUNICIPAL DO P|COMISSAO MUN PROV")] <- 0
    
    x$ind_osc[((str_detect(x$razao_social, "COMITE MUN") &
                !str_detect(x$razao_social, "ASSOC")) & 
            !str_detect(x$razao_social, "CIDADANIA")) & 
        !str_detect(x$razao_social, "APRENDIZ")] <- 0
    
    x$ind_osc[(str_detect(x$razao_social, "DIRETORIO REG") &
                  !str_detect(x$razao_social, "ESTUDANTES")) & 
        !str_detect(x$razao_social, "CONGADO")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "COMISSAO DIRETORA|COMISSAO DIRIGENTE|DIR\\. REGIONAL|COMIS PROVIS|DIRETORIO MUNC|ORGAO MUN")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "DIRETORIO -|DIRETORIO-|DIRETORIO PART|DIRET PARTIDO|PARTRIDO|DIRET PART|COMIS\\.PROV\\.|DIRETORIA PARTIDO|DIRETORIO MUNICILA|DIRETORIO MUNIPAL|DIRETORIO ESTADUAL|COMISSAO PRO MUN|DIRETORIO MUNICIP|DIRETO RIO|COMISAO PRO|DIRET ESTADUAL")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "DIRETORIO DEMO|DIRETORIO DE P|PSDB - DIRETORIO|PT DIRETORIO|PART DO MOV|DEMOCRATAS - DIRETORIO")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "PTB|PSDB|PMDB|^PT DE|DO PT DE|S PT DE|\\.PT DE|^PT D|PT DE C|-PT D|\\.PT D|^PSD| PSD$|PDT|^PSB|^PSB |^PRB |PRB-|^PPS |^PT |^PT\\.|^PMN |^PMN|PRTB|^PSL|^PSC |^PTC|^PTN|^PV |PPB|^PC DO|PC DO B$|PART COMUN|^P\\.P\\.B|P\\.H\\.S|P S B|P S D|P T|P D T|^P T B|P R T B|^P P|^P S L|PARTIDO DOS T|PART TRAB|MOV DEM|PART DEM|PART PROGR|^PFL| PFL|PARTIDO DA FR|^PR PAR|^PR -|^PR P|^PR S|^PR DE|^PR \\(|PARTIDO DA REP")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "PARTIDO D|PARTIDO DO MOVIM|PARTIDO DO MOVT|-PARTIDO|^PART |^PART\\.|^PPS|^DIRE\\.M|^DIRET\\.M|REG\\.PARTIDO|PARTIUDO|PARTDO")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, " PARTIDO ") &
                              !str_detect(x$razao_social, "GREMIO")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "DEMOCRATAS") & 
                              !str_detect(x$razao_social, "SINDICATO")] <- 0
    
    ## Remove labor unions and the like (sindicatos, federacoes e confederacoes) ==
    if(nrow(x)==694688 | nrow(x)==712806){

        x$ind_osc[str_detect(x$sbcl_cnae20, "9420100")] <- 0

    }
#     
#     else if(nrow(x)==709607){
#         
#         x$ind_osc[str_detect("9420100", x$CNAE.2.0.Subclasse)] <- 0
#         
#     }
#     
    else if(nrow(x)==716641){

        x$ind_osc[str_detect(as.character(x$cnae_2_0_subclasse), "9420100")] <- 0

    }
    
#     else if(nrow(x)==661601 | nrow(x)==669352){
#         
#         x$ind_osc[str_detect("9420100", x$SB.CLAS.20)] <- 0
#         
#     }
    
    x$ind_osc[str_detect(x$razao_social, "^SINDI|^SIND |^SIND\\.")] <- 0
    
    x$ind_osc[(((((str_detect(x$razao_social, " SIND ") &
                              !str_detect(x$razao_social, "ASS")) & 
                    !str_detect(x$razao_social, "GREMIO")) & 
                !str_detect(x$razao_social, "OCB")) &
            !str_detect(x$razao_social, "CLUBE")) & 
        !str_detect(x$razao_social, "SERV SOC"))] <- 0
    
    x$ind_osc[(str_detect(x$razao_social, " SINDICATO D") &
                  !str_detect(x$razao_social, "ASS")) & 
        !str_detect(x$razao_social, "COLONIA")] <- 0
    
    x$ind_osc[(((((((str_detect(x$razao_social, "^SIN") &
                                      !str_detect(x$razao_social, "^SINODO")) &
                            !str_detect(x$razao_social, "^SINAGOGA")) &
                        !str_detect(x$razao_social, "LTDA")) &
                    !str_detect(x$razao_social, "ASSOC")) &
                !str_detect(x$razao_social, "CONSULT")) &
            !str_detect(x$razao_social, "SINE")) & 
        !str_detect(x$razao_social, "SINVAL"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "SIND EMP|SINDICATO TRAB|SINDICATO PAT|SINDICATO ESP|SINDICATO IND|-SIND|SINDC\\.|SINDDOS|SINDEGETURES|SINDEMPCOMHORTRESTCHURPIZ|SIEMESP")] <- 0
    
    x$ind_osc[(((str_detect(x$razao_social, "SIND\\.D| SIND\\. D") &
                      !str_detect(x$razao_social, "ASSOC")) &
            !str_detect(x$razao_social, "CENTRO")) & 
        !str_detect(x$razao_social, "FEDERACAO"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^FED") & str_detect(x$razao_social, "SIND\\.")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "CONFED") & str_detect(x$razao_social, "SIND")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^FEDERACAO DOS SIN")] <- 0
    
    x$ind_osc[(((((((str_detect(x$razao_social, "^FED") &
                                      str_detect(x$razao_social, "SIND |SIND\\.")) & 
                            !str_detect(x$razao_social, "GREMIO")) & 
                        !str_detect(x$razao_social, "^ASSOC")) & 
                    !str_detect(x$razao_social, "^CENTRO")) & 
                !str_detect(x$razao_social, "^ASSC")) &
            !str_detect(x$razao_social, "^CEC")) & 
        !str_detect(x$razao_social, "^SERV"))] <- 0
    
    x$ind_osc[(str_detect(x$razao_social, "^FED") & 
                                        str_detect(x$razao_social, "TRAB")) & 
                              !str_detect(x$razao_social, "IMORTAL")] <- 0
    
    ## Remove condominiums =====================================================
    
    x$ind_osc[str_detect(x$razao_social, "^CONDOMINIO|^COND |^CON RES|^EDIFICIO CONDOMINIO|^EDIFICIO DO CONDOMINIO|^CONJUNTO RESID|^CONJ RES|^CONJ HAB|^CONJUNTO HAB|^NUCLEO HAB")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "119 CONDOMINIO DO EDIFICIO|ALLEGRO CONDOMINIO CLUB|ILHAS DO SUL CONDOMINIO NAUTICO|MORADIAS ABAETE 2 CONDOMINIO 8|VILLA CONDOMINIO MORRO DA ESPERANCA|A\\. CONDOMINIO RECANTO DO PARAISO|APRECEA CONDOMINIO COMARY GLEBA 6 A|HOTEL CAVALINHO BRANCO CONDOMINIO|ESPORTE CLUBE CONDOMINIO|MANUEL COSTA MELO CONDOMINIO|MARIO JOSE BORLINI CONDOMINIO")] <- 0
    
    ## Remove notary's offices (cartorios) =====================================
    
    x$ind_osc[str_detect(x$razao_social, "^CARTORIO|DE NOTAS$|^OFICIO|^REGISTRO|^SEGUNDO TAB")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "CARTORI") &
                              !str_detect(x$razao_social, "ASSOC")] <- 0
    
    ## Remove system S (sistema S) =============================================
    
    x$ind_osc[str_detect(x$razao_social, "^SENAI")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "SENAI$") & 
                              str_detect(x$razao_social, "^SERV")] <- 0
    
    x$ind_osc[(((str_detect(x$razao_social, " SENAI ") &
                !str_detect(x$razao_social, "ASSOC")) &
            !str_detect(x$razao_social, "AAPM")) &
        !str_detect(x$razao_social, "^ASS\\."))] <- 0
    
    x$ind_osc[(((((((((str_detect(x$razao_social, "SESI") &
                                              !str_detect(x$razao_social, "CONSELHO")) &
                                    !str_detect(x$razao_social, "ASSOC")) &
                                !str_detect(x$razao_social, "FARMACIA")) &
                            !str_detect(x$razao_social, "^APM")) &
                        !str_detect(x$razao_social, "^ASS")) &
                    !str_detect(x$razao_social, "^CENTRO")) & 
                !str_detect(x$razao_social, "A\\.P\\.M")) &
            !str_detect(x$razao_social, "FUNDACAO")) &
        !str_detect(x$razao_social, "CLUBE"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^SEBRAE")] <- 0
    
    x$ind_osc[(((str_detect(x$razao_social, " SEBRAE") &
                      !str_detect(x$razao_social, "ASSOC")) &
            !str_detect(x$razao_social, "ASSOSC")) &
        !str_detect(x$razao_social, "COLABORADORES"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^SENAR")] <- 0
    
    x$ind_osc[((str_detect(x$razao_social, "SENAR") &
                  !str_detect(x$razao_social, "ASSOCIACAO")) &
        !str_detect(x$razao_social, "ASSENAR"))] <- 0
    
    x$ind_osc[((((str_detect(x$razao_social, " SESC|^SESC") &
                          !str_detect(x$razao_social, "ASSOC")) &
                !str_detect(x$razao_social, "ASS\\.")) &
            !str_detect(x$razao_social, "GREMIO")) &
        !str_detect(x$razao_social, "INSTITUTO"))] <- 0
    
    x$ind_osc[((str_detect(x$razao_social, "SENAC") &
                  !str_detect(x$razao_social, "ASSOC")) &
        !str_detect(x$razao_social, "TEMPLO"))] <- 0
    
    ## Remove arbitration body (entidades de mediacao e arbitragem) ============
    x$ind_osc[((str_detect(x$razao_social, " MEDIACAO") &
                  !str_detect(x$razao_social, "ASS\\.")) &
        !str_detect(x$razao_social, "INSTIT\\."))] <- 0
    
    ## Remove conciliation commissions (comissao de conciliacao previa) ========
    
    x$ind_osc[str_detect(x$razao_social, "COMISSAO DE CON")] <- 0
    
    ## Remove city councils, consortia, and funds ==============================
    
    x$ind_osc[str_detect(x$razao_social, "^FUNDO MUN")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "CONSORCIO MUN")] <- 0
    
    ## Remove cemeteries and funeral homes =====================================
    
    x$ind_osc[str_detect(x$razao_social, "^CEMITER")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^FUNERAR")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "ASSOCIACAO FUNERARIA|CAIXA FUNERARIA")] <- 0
    
    ##  Remove SAs. LTDAs, and cooperatives ====================================
    
    x$ind_osc[str_detect(x$razao_social, "COOPERATIVA") & 
                              !str_detect(x$razao_social, "^ASS")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^COOP |^COOP\\.| S A$| S/A$| S\\.A$|LTDA|LTD$| S\\.A\\. | S/A")] <- 0
    
    x$ind_osc[(((str_detect(x$razao_social, "S\\.A\\.$") &
                  !str_detect(x$razao_social, "^ASSOC")) &
        !str_detect(x$razao_social, "^APMF")) & 
        !str_detect(x$razao_social, "^MITRA"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "RADIO TAXI") &
                              !str_detect(x$razao_social, "^AS")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, " LT$") & 
                  str_detect(x$razao_social, "^SAMMED|CONSULTORIA|COOP|PREST SERV|PARTICIPACOES|BIJUTERIAS|COMPANY|COMERCIO|PETROLEO")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "SOCIEDADE DE ENSINO SUPERIOR ESTACIO DE SA")] <- 0
    
    ## Remove pensions boards (caixas de previdencia) ==========================
    
    x$ind_osc[str_detect(x$razao_social, "CAIXA DE PREVIDENCIA|CAIXA DE PREV|CX PRE|CAIXA PREV")] <- 0
    
    ## Remove clinical analysis laboratories ===================================
    
    x$ind_osc[str_detect(x$razao_social, "SCMI LABORATORIO DE ANALISES CLINICAS|^LABORATORIO DE ANALISES CLINICAS")] <- 0
    
    ## Remove public agencies ==================================================
    
    x$ind_osc[str_detect(x$razao_social, "^SECRETARIA DE ESTADO|^SECRETARIA MUNICIPAL|^PREFEITURA COMUNITARIA|^PREFEITURA DA|^PREFEITURA DO SETOR|SECRETARIA DE ASSISTENCIA SOCIAL|SECRETARIA DA EDUCACAO")] <- 0
    
    ## Remove MEs e EPPs =======================================================
    
    x$ind_osc[str_detect(x$razao_social, "FUNDO DE INV|MARATHON EMERGING MARKETS FUND|TAEL INVESTMENTS (DELAWARE) LLC-MELLON|THE MAXIMA MULTPORTIFOLIO FUND LLC|TAEL INVESTMENTS (DELAWARE) LLC-MELLON|EPP$")] <- 0
    
    x$ind_osc[(((((((((((((((((((((((((((((((str_detect(x$razao_social, " ME$") &
                                                 !str_detect(x$razao_social, "LOC ITALIA$")) & 
                                            !str_detect(x$razao_social, "ASS DE")) & 
                                           !str_detect(x$razao_social, "ASSOC")) & 
                                          !str_detect(x$razao_social, "CONSELHO")) & 
                                         !str_detect(x$razao_social, "COMISSAO")) & 
                                        !str_detect(x$razao_social, "INSTITUTO")) &
                                       !str_detect(x$razao_social, "FUNDACAO")) &
                                      !str_detect(x$razao_social, "APMF")) &
                                     !str_detect(x$razao_social, "IGREJA")) &
                                    !str_detect(x$razao_social, "CONSORCIO")) &
                                   !str_detect(x$razao_social, "CONS ")) &
                                  !str_detect(x$razao_social, "CENTRO")) &
                                 !str_detect(x$razao_social, "NUCLEO")) &
                                !str_detect(x$razao_social, "MEMORIAL")) &
                               !str_detect(x$razao_social, "UNID EXEC")) &
                              !str_detect(x$razao_social, "^SOC")) &
                             !str_detect(x$razao_social, "^APM")) &
                            !str_detect(x$razao_social, "INST")) &
                           !str_detect(x$razao_social, "MINISTERIO")) &
                          !str_detect(x$razao_social, "^A AP P")) &
                         !str_detect(x$razao_social, "^ORG")) &
                        !str_detect(x$razao_social, "^CE ESC")) &
                       !str_detect(x$razao_social, "^ASS\\.")) &
                      !str_detect(x$razao_social, "^PATRONATO")) &
                     !str_detect(x$razao_social, "^ABRIGO")) &
                    !str_detect(x$razao_social, "^COMITE")) &
                   !str_detect(x$razao_social, "^CEC")) &
                  !str_detect(x$razao_social, "^COMUNIDADE")) &
                 !str_detect(x$razao_social, "^A P M F")) &
                !str_detect(x$razao_social, "^APP")) &
               !str_detect(x$razao_social, "^A\\.P\\.P\\."))] <- 0
    
    ## Remove L.P., LTD, Banco/Bank, INC., S/A, SA, S.A. =======================
    
    if(nrow(x)==694688 | nrow(x)==712806){
        
        x$ind_osc[str_detect(x$nat_jur2009, "3212") & str_detect(x$razao_social, " SA ")] <- 0
        
    }
    
    else if(nrow(x)==709607){
        
        x$ind_osc[str_detect(x$Natureza.Jurídica, "3212") & str_detect(x$razao_social, " SA ")] <- 0
        
    }
    
    else if(nrow(x)==716641){
        
        x$ind_osc[str_detect(x$natureza_juridica, "3212") & str_detect(x$razao_social, " SA ")] <- 0
        
    }
    
    else if(nrow(x)==661601 | nrow(x)==669352){
        
        x$ind_osc[str_detect(x$NAT.JURIDICA, "3212") & str_detect(x$razao_social, " SA ")] <- 0
        
    }
    
    x$ind_osc[((((str_detect(x$razao_social, " S\\.A\\.") &
                  !str_detect(x$razao_social, "^ASSOC")) &
        !str_detect(x$razao_social, "^ASS\\.")) &
        !str_detect(x$razao_social, "^PROJETO")) &
        !str_detect(x$razao_social, "^BLOCO"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, " L\\.P\\.") &
                  !str_detect(x$razao_social, "^LIGA")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "SOUZA CRUZ SA")] <- 0
    
    x$ind_osc[((str_detect(x$razao_social, " S/A") &
                                      !str_detect(x$razao_social, "^ASSOC")) &
                            !str_detect(x$razao_social, "^GREMIO"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, " LTD|CTVM|DTVM")] <- 0
    
    x$ind_osc[(((((((((str_detect( x$razao_social, " LP") &
                                              !str_detect(x$razao_social, "^ASSOC")) & 
                                    !str_detect(x$razao_social, "^LIGA")) & 
                                !str_detect(x$razao_social, "^MINISTERIO")) & 
                    !str_detect(x$razao_social, "^LIRICA")) & 
                   !str_detect(x$razao_social, "^IGREJA")) &
                  !str_detect(x$razao_social, "CENTRO")) & 
                 !str_detect(x$razao_social, "^CLUBE")) &
                !str_detect(x$razao_social, "^APP")) &
               !str_detect(x$razao_social, "^ASES"))] <- 0
    
    x$ind_osc[(((str_detect(x$razao_social, " L\\.P") &
                  !str_detect(x$razao_social, "^ASSOC")) & 
        !str_detect(x$razao_social, "^LIGA")) & 
        !str_detect(x$razao_social, "^CONS"))] <- 0
    
    x$ind_osc[((str_detect(x$razao_social, " LDC") & 
                                      !str_detect(x$razao_social, "^LIGA")) & 
                            !str_detect(x$razao_social, "^AMMP"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, " LLC") & 
                            !str_detect(x$razao_social, "^LIGA")] <- 0
    
    x$ind_osc[((((((((((((((((str_detect(x$razao_social, "INVEST") &
                                  !str_detect(x$razao_social, "INSTIT")) & 
                             !str_detect(x$razao_social, "^INST")) & 
                            !str_detect(x$razao_social, "INVESTIG")) & 
                           !str_detect(x$razao_social, "COMUNITARIA")) & 
                          !str_detect(x$razao_social, "ASSOC")) & 
                         !str_detect(x$razao_social, "CLUBE")) &
                        !str_detect(x$razao_social, "GRUPO")) &
                       !str_detect(x$razao_social, "^REDE")) &
                      !str_detect(x$razao_social, "^FUNDACAO")) &
                     !str_detect(x$razao_social, "CONSELHO")) &
                    !str_detect(x$razao_social, "BENEFIC")) &
                   !str_detect(x$razao_social, "SOCIEDADE")) &
                  !str_detect(x$razao_social, "^CENTRO")) &
                 !str_detect(x$razao_social, "^CIRCULO")) &
                !str_detect(x$razao_social, "^COMITE")) &
               !str_detect(x$razao_social, "^TEATRO"))] <- 0
    
    ## Remove organizations of other types =====================================
    
#     x$ind_osc[str_detect("1512", x$clas_cnae20)] <- 0
#     x$ind_osc[str_detect("93298", x$clas_cnae20)] <- 0
#     x$ind_osc[str_detect("43991", x$clas_cnae20)] <- 0

    x$ind_osc[str_detect(x$razao_social, "ESPOLIO")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^FAZ")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^EDIFICIO RESIDENCIAL")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^JOSE")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^JOAO")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^MARIA")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^ESC") & str_detect(x$razao_social, " MUN")] <- 0
    
    x$ind_osc[((((((str_detect(x$razao_social, "^ESCO") &
                                  str_detect(x$razao_social, " EST")) & 
                       !str_detect(x$razao_social, "ESTUDOS")) & 
                      !str_detect(x$razao_social, "SAMBA")) & 
                     !str_detect(x$razao_social, "CICLISMO")) & 
                    !str_detect(x$razao_social, "MEDIUNS")) &
                   !str_detect(x$razao_social, "ESCOLA EVANGELISTA ESTAVAO SLOOP"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^EMBAIXADA D")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "BANKBOSTON|CITIBANK") & !str_detect(x$razao_social, "CITIBANK CLUB")] <- 0
    
    if(nrow(x)==694688 | nrow(x)==712806){
        
        x$ind_osc[str_detect(x$nat_jur2009, "3212") &
                                str_detect(x$razao_social, "BANCO|CREDIT|FUND|TRUST| INC|CAPITAL|MARKET|PORTFOLIO|S\\.A|COMPANY|PARTNER|HOLDING|LIMITED|RETIRE| PENS|BANKB|BANK|CITIB|FINANC|INSURANCE|CORPORATION|EQUITY|GLOBAL|GROWTH|GOVERNM|RET$")] <- 0
        
        x$ind_osc[str_detect(x$nat_jur2009, "3212") & str_detect(x$sbcl_cnae20, "6470101|6541300")] <- 0
        
        x$ind_osc[str_detect(x$nat_jur2009, "3212") &
                                str_detect(x$razao_social, "ASSET")] <- 0
        
        x$ind_osc[str_detect(x$nat_jur2009, "3212") &
                                str_detect(x$razao_social, "MORGAN")] <- 0
        
        x$ind_osc[str_detect(x$nat_jur2009, "3212") & 
                                str_detect(x$razao_social, "BNP|ITAU|BCH|BSN|ACCIONES|S\\. A\\.|CO\\.|FIM|BBV|AXA|BANQUE|F\\.I\\.M|SICAV|PLC|ACTIONS|FONDS|PRUDENTIAL|UBS|MERRILL|EQUIT")] <- 0
        
        x$ind_osc[str_detect(x$nat_jur2009, "3212") &
                                str_detect(x$razao_social, "FCP|AIM|AMERICAN CENTURY|MONETARY")] <- 0
        
    }
    
    else if(nrow(x)==709607){
        
        x$ind_osc[str_detect(x$Natureza.Jurídica, "3212") &
                                str_detect(x$razao_social, "BANCO|CREDIT|FUND|TRUST| INC|CAPITAL|MARKET|PORTFOLIO|S\\.A|COMPANY|PARTNER|HOLDING|LIMITED|RETIRE| PENS|BANKB|BANK|CITIB|FINANC|INSURANCE|CORPORATION|EQUITY|GLOBAL|GROWTH|GOVERNM|RET$")] <- 0
        
        x$ind_osc[str_detect(x$Natureza.Jurídica, "3212") & str_detect(x$CNAE.2.0.Subclasse, "6470101|6541300")] <- 0
        
        x$ind_osc[str_detect(x$Natureza.Jurídica, "3212") & str_detect("ASSET", x$razao_social)] <- 0
        
        x$ind_osc[str_detect(x$Natureza.Jurídica, "3212") &
                                str_detect(x$razao_social, "MORGAN")] <- 0
        
        x$ind_osc[str_detect(x$Natureza.Jurídica, "3212") &
                                str_detect(x$razao_social, "BNP|ITAU|BCH|BSN|ACCIONES|S\\. A\\.|CO\\.|FIM|BBV|AXA|BANQUE|F\\.I\\.M|SICAV|PLC|ACTIONS|FONDS|PRUDENTIAL|UBS|MERRILL|EQUIT")] <- 0
        
        x$ind_osc[str_detect(x$Natureza.Jurídica, "3212") &
                                str_detect(x$razao_social, "FCP|AIM|AMERICAN CENTURY|MONETARY")] <- 0
        
    }
    
    else if(nrow(x)==716641){
        
        x$ind_osc[str_detect(x$natureza_juridica, "3212") &
                                str_detect(x$razao_social, "BANCO|CREDIT|FUND|TRUST| INC|CAPITAL|MARKET|PORTFOLIO|S\\.A|COMPANY|PARTNER|HOLDING|LIMITED|RETIRE| PENS|BANKB|BANK|CITIB|FINANC|INSURANCE|CORPORATION|EQUITY|GLOBAL|GROWTH|GOVERNM|RET$")] <- 0
        
        x$ind_osc[str_detect(x$natureza_juridica, "3212") & str_detect(x$cnae_2_0_subclasse, "6470101|6541300")] <- 0
        
        x$ind_osc[str_detect(x$natureza_juridica, "3212") & 
                                str_detect(x$razao_social, "ASSET")] <- 0
        
        x$ind_osc[str_detect(x$natureza_juridica, "3212") &
                                str_detect(x$razao_social, "MORGAN")] <- 0
        
        x$ind_osc[str_detect(x$natureza_juridica, "3212") &
                                str_detect(x$razao_social, "BNP|ITAU|BCH|BSN|ACCIONES|S\\. A\\.|CO\\.|FIM|BBV|AXA|BANQUE|F\\.I\\.M|SICAV|PLC|ACTIONS|FONDS|PRUDENTIAL|UBS|MERRILL|EQUIT")] <- 0
        
        x$ind_osc[str_detect(x$natureza_juridica, "3212") &
                                str_detect(x$razao_social, "FCP|AIM|AMERICAN CENTURY|MONETARY")] <- 0
        
    }
    
    else if(nrow(x)==661601 | nrow(x)==669352){
        
        x$ind_osc[str_detect(x$NAT.JURIDICA, "3212") & 
                                str_detect(x$razao_social, "BANCO|CREDIT|FUND|TRUST| INC|CAPITAL|MARKET|PORTFOLIO|S\\.A|COMPANY|PARTNER|HOLDING|LIMITED|RETIRE| PENS|BANKB|BANK|CITIB|FINANC|INSURANCE|CORPORATION|EQUITY|GLOBAL|GROWTH|GOVERNM|RET$")] <- 0
        
        x$ind_osc[str_detect(x$NAT.JURIDICA, "3212") & str_detect(x$SB.CLAS.20, "6470101|6541300")] <- 0
        
        x$ind_osc[str_detect(x$NAT.JURIDICA, "3212") & 
                                str_detect(x$razao_social, "ASSET")] <- 0
        
        x$ind_osc[str_detect(x$NAT.JURIDICA, "3212") & 
                                str_detect(x$razao_social, "MORGAN")] <- 0
        
        x$ind_osc[str_detect(x$NAT.JURIDICA, "3212") & 
                                str_detect(x$razao_social, "BNP|ITAU|BCH|BSN|ACCIONES|S\\. A\\.|CO\\.|FIM|BBV|AXA|BANQUE|F\\.I\\.M|SICAV|PLC|ACTIONS|FONDS|PRUDENTIAL|UBS|MERRILL|EQUIT")] <- 0
        
        x$ind_osc[str_detect(x$NAT.JURIDICA, "3212") & 
                                str_detect(x$razao_social, "FCP|AIM|AMERICAN CENTURY|MONETARY")] <- 0
        
    }
    
    x$ind_osc[str_detect(x$razao_social, "PARTIDO") &
                            !str_detect(x$razao_social, "^ASS")] <- 0
    
    x$ind_osc[((str_detect(x$razao_social, "CAIXA") & 
                                      str_detect(x$razao_social, "PREV")) &
                            !str_detect(x$razao_social, "CASPREV"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "CX") &
                            str_detect(x$razao_social, "PREV")] <- 0
    
    x$ind_osc[((str_detect(x$razao_social, "CX") &
                                      str_detect(x$razao_social, "ESC")) &
                            !str_detect(x$razao_social, "ATECCX"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^CONDOM|CERES CONDOMINIO|VIVER CONDOMINIO|O QUINTAL CONDOMINIO|BRASILIA CONDOMINIOS|SOCIEDADE DO CONDOMINIO|SOUSA CONDOMINIO|FAZ-CONDOMINIO ALEGRE|CONDOMINIO DO CONJUNTO PARQUE")] <- 0
    
    x$ind_osc[(((((((((str_detect(x$razao_social, "^COOP") &
                                              !str_detect(x$razao_social, "COOPERACAO")) & 
                          !str_detect(x$razao_social, "COOPERADORES")) & 
                         !str_detect(x$razao_social, "COOPERART")) & 
                        !str_detect(x$razao_social, "FUTEBOL CLUBE")) & 
                       !str_detect(x$razao_social, "INSTITUTO")) &
                      !str_detect(x$razao_social, "COOPERAZIONE")) & 
                     !str_detect(x$razao_social, "COOPERATIVUS")) &
                    !str_detect(x$razao_social, "COOPLANTIO SOLIDARIA")) &
                   !str_detect(x$razao_social, "COOPERCICLA"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "COMISSAO ORG") & 
                            str_detect(x$razao_social, "PART")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "HSBC FINANCIE")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^ARIES")] <- 0
    x$ind_osc[str_detect(x$razao_social, "FIDELITY ADVISOR")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "^ESC EST")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^COLEGIO EST|^COL EST")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^COLEGIO MUN")] <- 0
    x$ind_osc[str_detect(x$razao_social, "^E\\.M\\.E\\.F\\.|^EMEF|^EEEF")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "FUNDO MUN")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "CONSELHO MUN") & 
                            !str_detect(x$razao_social, "MORGAN")] <- 0
    
    x$ind_osc[(((((str_detect(x$razao_social, "CONSELHO MUN") & 
                              !str_detect(x$razao_social, "ASSOC")) & 
                    !str_detect(x$razao_social, "PAST")) & 
                !str_detect(x$razao_social, "IGREJA")) & 
            !str_detect(x$razao_social, "DA CRECHE")) &
        !str_detect(x$razao_social, "MUNIC HUMANITARIO"))] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "CAMARA ARBITRAL|TRIBUNAL ARBITRAL|FORUM ARBITRAL|JUIZ ARBITRAL|JUSTICA ARBITRAL|TRIBUNAL ARBITRAL|JUIZADO ESPECIAL ARBITRAL|JUIZADO ARBITRAL|MEDIACAO ARBITRAL|TRIB ARBITRAL|CONSELHO ARBITRAL|CAMARA DE ARBITRAGEM|MED E ARBITRAGEM|CONCILIACAO E MED|MEDICACAO ARBITRAL|MEDIACAO E ARBITRAGEM") &
                            !str_detect(x$razao_social, "^ASS")] <- 0
    
    x$ind_osc[(str_detect(x$razao_social, "TRIBUNAL") &
                                      str_detect(x$razao_social, "ARBIT")) &
                            !str_detect(x$razao_social, "^ASS")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "CORTE") &
                            str_detect(x$razao_social, "ARBIT")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "CONCILIACAO") &
                            str_detect(x$razao_social, "ARBITRAGEM")] <- 0
    
    x$ind_osc[str_detect(x$razao_social, "ASSOCIACAO ARBITRAL")] <- 0
    
    x <- x %>%
        mutate(ind_osc = ifelse(tipo_estab==3, 0, ind_osc))
    
    # Remove individual businessman ============================================
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^NELSON","^OSVALDO","^SALIM","^TEODOMIRO",
                                                                 "^SEBASTIAO","^PRODUTORI DI LATTE","^ROMUALDO","^ANTONIO", 
                                                                 "^CATIA","^ADALSIDNEI","^GIOVANI","^LUIZA","^SIDINEI","^MOISES",
                                                                 "^ADILSON","^JOSE","^MARIA","^ANTONIO","^ANA" ,"^ANA\\.","^JOAO",
                                                                 "^FRANCISCO","^CARLOS","^PAULO","^PEDRO","^LUCAS","^LUIZ",
                                                                 "^MARCOS","^LUIS","^GABRIEL","^RAFAEL","^FRANCISCA" ,"^FRANCISCA\\.","^DANIEL",
                                                                 "^MARCELO","^BRUNO","^EDUARDO","^ANTONIA","^MARCIA","^ADRIANA",
                                                                 "^JULIANA","^PATRICIA","^ALINE","^FERNANDA","^SANDRA","^CAMILA",
                                                                 "^MOACIR", "^CINTIA", "^TAILIZE", "^ALCIMARA", "^ANICE", "^LIMA E ROCHA",
                                                                 "^SC FERNANDES", "^NILCE", "^IVA", "^IVANI", "^IVAN", "^TANIA",
                                                                 "^SILVESTRE", "^TANIA", "^R ELIZEANEA", "^LUZITANIA", "^ELIANE",
                                                                 "^TIAGO", "^ANDRE-", "^ANDRESSA", "^ANDRE\\.", "^ANDREA", "^LAURA",
                                                                 "^NASARE", "^HELOISA", "^MARCO ANTONIO", "^TADEU", "^ALEXANDRA", 
                                                                 "^NILSON", "^CLEBSON", "^DEBORA", "^DAVI", "^SUELI", "^ROBERTO", "^LEONARDO",
                                                                 "^DANILO","^ROBERSON", "^VITORINA", "^CENIRA", "^NORMELINO", "^A.L PEREIRA",
                                                                 "^E M SILVEIRA", "^AILF CLARO", "^CECILIA", "^SERGIO",
                                                                 "^CLAUDIA", "^ILZA", "^GERALDO", "^LUCIANA", "^HELENITA",
                                                                 "^KARINE", "^MAURICIO", "^MANOEL", "^LUCIANO", "^ALCIR"), collapse = "|")) & 
                                    !str_detect(razao_social, paste(c("ASSO","ORG","CLUBE","HUMANAS","ADVOGADOS"), collapse = "|")),
                                0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, "FUNDO PREVID|^INSTITUTO DE PREV") & 
                                    str_detect(razao_social, "MUNIC"), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, "^CONSULADO DE|^CONSULADO G"), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(x$cnpj_cei, "03738996000125|22224224000110"), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, "CAIXA ESCOLAR") & 
                                    !(str_detect(razao_social, "ASSOS")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse((str_detect(razao_social, "^SERVICO SOCIAL DA IND") |
                                     str_detect(razao_social, "^SERVICO SOCIAL DO COMERCIO")) &
                                    !str_detect(razao_social, "CONST"), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, "SERVICO NACIONAL DE APRENDIZAGEM") |
                                    str_detect(razao_social, "SERV\\.DE APOIO") | 
                                    str_detect(razao_social, "SERV NAC DE APR"), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("CAIXA CHAVESESCOLAR", "CAIXAR ESCOLAR", "^CAIXA  ESCOLAR",
                                                                 "CAIXA ESOCLAR", "CAIXA ESOLAR", "CAIXAESC", "CAIXA ECOLAR",
                                                                 "^CAIXA EXECUTORA", "CAIXA ESACOLAR", "^CAIXAS ESC", "CAIXA DE ENSINO",
                                                                 "CAIXA  ESC", "CAIXA ECSOLAR", "CAIXA DA ESCOLA", "CAIXA EXCOLAR",
                                                                 "CAIXAESCOLAR", "CAIXA EDSCOLAR", "^CAIXA$",
                                                                 "CAIXAESCOLARCORONELCARNEIROJUNIOR", "CAIXA MUNICIPAL",
                                                                 "^CAIXA ECONOMICA", "CAIXA DO PRE ESCOLAR", "CAIXAA ESCOLAR",
                                                                 "CAIXAESCOLAR", "CAIXA CANDIDA", "CAIXAE ESCOLAR", "CAIXA SECOLAR",
                                                                 "CAIXA UMEI", "CAIXA DA ESC", "CAIXA DE CUSTEIO", "CAIXA EDCOLAR",
                                                                 "CAIXA DA E\\. E\\.", "CAIXA ES\\.DA", "CAIXA  ES ESC",
                                                                 "CAIXA EEEEF", "CAIXA EE", "CAIXA PROF\\.", "CAIXA LESCOLAR"), collapse = "|")),
                                0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("COMI\\. PROV\\. DO", "COM\\. EXEC\\. MUN\\.", "D\\. PART\\.DO", 
                                                                 "CONDOINIO", "CONDO  ", "CONDONIMIO"), collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^FUN MUN", "^CONS MUN", "^CONS\\.MUN"), collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, "^EMBAIXADA REAL"), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^PET SHOP", "^PREMIUM VIDEO COMERCIAL", "^JOLY COMERCIO", "SALVASHOPPING", 
                                                                 "BRESHOPPING", "^CIAL AGROMEN", "LIVRARIA DOM COSTA", "EDUCA PRODUTOS E SERVICOS", 
                                                                 "- PRODUTOS E SERVICOS", "^R\\.A\\.P", "BETTY CENTER", "NORTE MEDICAL CENTER", "REDE LOCAL IMOVEIS",
                                                                 "^CENTRO COMERCIAL", "URCA IMOBILIARIA", "EMPREENDIMENTO POUSADA"), collapse = "|")), 0, ind_osc))
    
    x <- mutate(x, razao_social = str_to_title(razao_social))
    
    # Remove Parent-teacher association, school boards etc. (APM, conselhos etc.) ====
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Associacao De Pais E Pr", "^Associacao De Pa", "^Associacao D Pa", "^Associacao Pais",
                                                                 "^Associacao P E M", "^Associacao P M ", "^Associacao P M", "^Associacao P P", "^Associacao Pais,",
                                                                 "^Associacao P\\.", "^Associacao De Pais",
                                                                 
                                                                 "^Assoc Pais E M", "^Assoc Pais E Pr", "^Assoc Pais Pr", "^Assoc Pais M", "^Assoc P M ", "^Assoc P P",
                                                                 "^Assoc P Prof", "^Assoc P,", "^Assoc Pais,", "^Assoc\\. P", "^Assoc\\.P", "Assoc De Pais",
                                                                 
                                                                 "^Asso Pais E M", "^Asso Pais P", "^Asso Pais E P", "^Asso\\. Pais P", "^^Asso De Pais",
                                                                 
                                                                 "^Ass De Pais E Mestre", "^Ass Pais,", "^Ass De Pais", "^Ass De Pais, Mestres", "^Ass De Pais Mest", 
                                                                 "^Ass De Pais E P", "^Ass De Pais Me", "^Ass De Pais E M ", "^Ass De Pais,Mestr", 
                                                                 "^Ass Pais E P", "^Ass Pais Mest", "^Ass Pais E Mestres", "^Ass Pais M Esc", "^Ass P M",
                                                                 "^Ass P E M", "^Ass P M E", "^Ass P Mestre", "^Ass P P", "^Ass\\. P", "Ass\\.P", 
                                                                 "^Ass\\. De",
                                                                 
                                                                 "^A P M", "^A P M C", "^Apm", "^App", "^Aap", "^As Pais Pr", "^As Pais E M", "^As Pais M", 
                                                                 "^As Pais E Pr", "^Associacaode Pais", "^A\\.P\\.P", "^A\\.A\\.P", "^A\\.P\\.M", "^As\\.Pais",
                                                                 
                                                                 "^Cpm", "^Circ  De Pais", "^Circ De Pais", "^C\\.P\\.M", "Apm Da Ee",
                                                                 "Apm Ee", "Apm Da E E", "Assoc\\.De Pais E M\\."), collapse = "|")) &
                                    str_detect(razao_social, paste(c("Esc Est", "Escola Est", "Escola Mun", "Esc Mun", "Esc M ", 
                                                                     "Esc E ", "Emef", "E M E F", "Eeef", "E E E F", "Emei", "E M E I", 
                                                                     "E E E I", "E Est", "E Mun",
                                                                     
                                                                     "Esc\\.Est", "Esc\\. Est", "Escola Est", "Escola Mun", "Esc\\.Mun", "Esc\\. Mun", "Esc Mun", 
                                                                     "Esc\\. M ", "Esc\\. E", "Esc E ", "Emef", "E M E F", "Eeef", "E E E F", "Emei", "E M E I", 
                                                                     "E E E I", "E Est", "E Mun", "E\\.M\\.E\\.F", "E\\. M\\. E\\. F", "E\\.M\\.E\\.I",
                                                                     "E\\. M\\. E\\. I", "E\\. Mun", "E\\.Mun", "E\\.Est", "E\\. Est", "Dom Pedro Ii",
                                                                     "D Pedro Ii", "D\\.Pedro Ii", "D\\. Pedro Ii"),
                                                                   collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Associacao De Pais E Pr", 
                                                                 "^Associacao P E M", "^Associacao P M ", "^Associacao P M", "^Associacao P P", 
                                                                 
                                                                 "^Assoc Pais E M", "^Assoc Pais E Pr", "^Assoc Pais Pr", "^Assoc Pais M", "^Assoc P M ", "^Assoc P P",
                                                                 "^Assoc P Prof", "^Assoc P,", 
                                                                 
                                                                 "^Asso Pais E M", "^Asso Pais P", "^Asso Pais E P", "^Asso\\. Pais P", 
                                                                 
                                                                 
                                                                 "^Ass De Pais E Mestre",
                                                                 "^Ass De Pais, Mestres", "^Ass De Pais Mest", 
                                                                 "^Ass De Pais E P", "^Ass De Pais Me", "^Ass De Pais E M ", "^Ass De Pais,Mestr", 
                                                                 "^Ass Pais E P", "^Ass Pais Mest", "^Ass Pais E Mestres", "^Ass Pais M Esc", "^Ass P M",
                                                                 "^Ass P E M", "^Ass P M E", "^Ass P Mestre", "^Ass P P", 
                                                                 
                                                                 "^A P M", "^A P M C", "^Apm", "^App", "^Aap", "^As Pais Pr", "^As Pais E M", "^As Pais M", 
                                                                 "^As Pais E Pr", "^Associacaode Pais", "^A\\.P\\.P", "^A\\.A\\.P", "^A\\.P\\.M", 
                                                                 
                                                                 "^Cpm", "^Circ  De Pais", "^Circ De Pais", "^C\\.P\\.M", "Apm Da Ee",
                                                                 "Apm Ee", "Apm Da E E", "^Assoc\\.De Pais E M\\."), collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Associacao De Pa", "^Associacao D Pa", "^Associacao Pais", 
                                                                 "^Associacao Pais,", "^Associacao P\\.", "^Associacao De Pais",
                                                                 "^Assoc Pais,", "^Assoc\\. P", "^Assoc\\.P", "^Assoc De Pais", 
                                                                 "^Asso De Pais", "^Ass Pais,", "^Ass De Pais", "^Ass\\. P", "Ass\\.P", 
                                                                 "^Ass\\. De", "^As\\.Pais"), collapse = "|")) &
                                    str_detect(razao_social, paste(c("Mestres", "Mest", "Prof", "Escola", "Colegio", "Esc", "Col", 
                                                                     "Creche", "Centro Educ"), collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("Conselhoescolar", "^Conselho Esc", "^Conselho E ", "^Conselho E\\.", 
                                                                 "^Conse Esc", "^Conse\\. Esc", "^Conse\\.Esc", "^Conse E ", 
                                                                 "^Conse\\.E ", "^Conse E\\.", "^Consel Esco", "^Consel\\. Esco", 
                                                                 "^Consel\\.Esco", "^Consel\\. E ", "^Consel E ", "^Consel E\\.", 
                                                                 "^Consel\\.E ", "^Conselh Esc", "^Conselh E ", "^Cons Esc", "^Cons\\.Esc",
                                                                 "^Cons\\. Esc", "^Cons E ", "^Cons\\.E ", "^Cons\\. E ", "^Con Esc",
                                                                 "^Con\\.Esc", "^Con\\. Esc", "^Co Esc", "^Co\\.Esc", "^Co\\. Esc"), collapse = "|")) &
                                    str_detect(razao_social, paste(c("Esc Est", "Escola Est", "Escola Mun", "Esc Mun", "Esc M ", 
                                                                     "Esc E ", "Emef", "E M E F", "Eeef", "E E E F", "Emei", "E M E I", 
                                                                     "E E E I", "E Est", "E Mun",
                                                                     
                                                                     "Esc\\.Est", "Esc\\. Est", "Escola Est", "Escola Mun", "Esc\\.Mun", "Esc\\. Mun", "Esc Mun", 
                                                                     "Esc\\. M ", "Esc\\. E", "Esc E ", "Emef", "E M E F", "Eeef", "E E E F", "Emei", "E M E I", 
                                                                     "E E E I", "E Est", "E Mun", "E\\.M\\.E\\.F", "E\\. M\\. E\\. F", "E\\.M\\.E\\.I",
                                                                     "E\\. M\\. E\\. I", "E\\. Mun", "E\\.Mun", "E\\.Est", "E\\. Est", "Dom Pedro Ii",
                                                                     "D Pedro Ii", "D\\.Pedro Ii", "D\\. Pedro Ii"),
                                                                   collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("Conselhoescolar", "^Conselho Esc", "^Conselho E ", "^Conselho E\\.", 
                                                                 "^Conse Esc", "^Conse\\. Esc", "^Conse\\.Esc", "^Conse E ", 
                                                                 "^Conse\\.E ", "^Conse E\\.", "^Consel Esco", "^Consel\\. Esco", 
                                                                 "^Consel\\.Esco", "^Consel\\. E ", "^Consel E ", "^Consel E\\.", 
                                                                 "^Consel\\.E ", "^Conselh Esc", "^Conselh E ", "^Cons Esc", "^Cons\\.Esc",
                                                                 "^Cons\\. Esc", "^Cons E ", "^Cons\\.E ", "^Cons\\. E ", "^Con Esc",
                                                                 "^Con\\.Esc", "^Con\\. Esc", "^Co Esc", "^Co\\.Esc", "^Co\\. Esc"), collapse = "|")), 
                                0, ind_osc))
    
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Conselho Del", "^Conselho D ", "^Conse Del", "^Conse D ", "^Consel Del",
                                                                 "^Consel D ", "^Conselh Del", "^Conselh D ", "^Cons Del", "^Cons D ",
                                                                 "^Con Del", "^Co Del", "^C D "), collapse = "|")) &
                                    str_detect(razao_social, paste(c("Esc Est", "Escola Est", "Escola Mun", "Esc Mun", "Esc M ", 
                                                                     "Esc E ", "Emef", "E M E F", "Eeef", "E E E F", "Emei", "E M E I", 
                                                                     "E E E I", "E Est", "E Mun",
                                                                     
                                                                     "Esc\\.Est", "Esc\\. Est", "Escola Est", "Escola Mun", "Esc\\.Mun", "Esc\\. Mun", "Esc Mun", 
                                                                     "Esc\\. M ", "Esc\\. E", "Esc E ", "Emef", "E M E F", "Eeef", "E E E F", "Emei", "E M E I", 
                                                                     "E E E I", "E Est", "E Mun", "E\\.M\\.E\\.F", "E\\. M\\. E\\. F", "E\\.M\\.E\\.I",
                                                                     "E\\. M\\. E\\. I", "E\\. Mun", "E\\.Mun", "E\\.Est", "E\\. Est", "Dom Pedro Ii",
                                                                     "D Pedro Ii", "D\\.Pedro Ii", "D\\. Pedro Ii"),
                                                                   collapse = "|")), 
                                0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Conselho Del", "^Conselho D ", "^Conse Del", "^Conse D ", "^Consel Del",
                                                                 "^Consel D ", "^Conselh Del", "^Conselh D ", "^Cons Del", "^Cons D ",
                                                                 "^Con Del", "^Co Del", "C D C", "C D Da"), collapse = "|")), 0, ind_osc))
    
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^U E", "^U E ", "^U\\.E ", "^U\\.E", "^Unidade E", "^Uni Ex", "^Uni\\.Ex",
                                                                 "^Uni\\. Ex", "^Unid Ex", "^Unid\\.Ex", "^Unid\\. Ex", "^Un Ex", "^Un\\.Ex",
                                                                 "^Un\\. Ex", "^U Ex", "^U\\.Ex", "^U\\. Ex", "^C E Da Esc", "^U\\.Exe",
                                                                 "^Comite Exe", "^Com\\. Exe", "^Com\\.Exe", "^Com\\.E", "^Com\\. E",
                                                                 "^Co\\. E", "^C\\. E", "^C\\.E", "^C E"), collapse = "|")) &
                                    str_detect(razao_social, paste(c("Esc Est", "Escola Est", "Escola Mun", "Esc Mun", "Esc M ", 
                                                                     "Esc E ", "Emef", "E M E F", "Eeef", "E E E F", "Emei", "E M E I", 
                                                                     "E E E I", "E Est", "E Mun",
                                                                     
                                                                     "Esc\\.Est", "Esc\\. Est", "Escola Est", "Escola Mun", "Esc\\.Mun", "Esc\\. Mun", "Esc Mun", 
                                                                     "Esc\\. M ", "Esc\\. E", "Esc E ", "Emef", "E M E F", "Eeef", "E E E F", "Emei", "E M E I", 
                                                                     "E E E I", "E Est", "E Mun", "E\\.M\\.E\\.F", "E\\. M\\. E\\. F", "E\\.M\\.E\\.I",
                                                                     "E\\. M\\. E\\. I", "E\\. Mun", "E\\.Mun", "E\\.Est", "E\\. Est", "Dom Pedro Ii",
                                                                     "D Pedro Ii", "D\\.Pedro Ii", "D\\. Pedro Ii"),
                                                                   collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^U E", "^U E ", "^U\\.E ", "^U\\.E", "^Unidade E", "^Uni Ex", "^Uni\\.Ex",
                                                                 "^Uni\\. Ex", "^Unid Ex", "^Unid\\.Ex", "^Unid\\. Ex", "^Un Ex", "^Un\\.Ex",
                                                                 "^Un\\. Ex", "^U Ex", "^U\\.Ex", "^U\\. Ex", "^C E Da Esc", "^U\\.Exe",
                                                                 "^Comite Exe", "^Com\\. Exe", "^Com\\.Exe", "^Com\\.E", "^Com\\. E",
                                                                 "^Co\\. E", "^C\\. E", "^C\\.E", "^C E"), collapse = "|")) &
                                    !str_detect(razao_social, paste(c("Comunidade", "Evangelica", "Espirita", "Evan\\.", "Evang", "Luterana", "Ogum",
                                                                      "Confissao", "Templo", "Terapeutico", "Profet", "Graca De Deus", "C\\.E\\.B\\.U\\.V\\.",
                                                                      "Centro Esp\\.", "Com\\. Ev\\.", "C E B U V", "Ministerio", "Enferm Santa Casa",
                                                                      "Recuperacao", "Envelhecimento"), collapse = "|")), 0, ind_osc))
    
    
    ## Remove labor union ======================================================
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, "^Simesgo$"), 0, ind_osc))
    
    ## Remove system S =========================================================
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, "^Servico De Apoio As Micro E Pequenas"), 0, ind_osc))
    
    ## Remove notaries =========================================================
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("Servico Notarial", "Cart Civil",
                                                                 "Tabelionato", "Juizado Especial",
                                                                 "Itapuca Oficio", "Tab Notas", 
                                                                 "Tab De Not", " E Notas", 
                                                                 "1 Oficio Notas"), 
                                                               collapse = "|")), 0, ind_osc))
    
    ## Remove public agencies ==================================================
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Secretaria Do Estado De Educacao",
                                                                 "^Sec De Estado De Educ ",
                                                                 "^Sec De Estado De Edc ",
                                                                 "^Sec De Estado De Edeucacao",
                                                                 "Municipio De Estancia"), 
                                                               collapse = "|")), 0, ind_osc))
    
    ## Remove Parent-teacher association, school boards etc. (APM, conselhos etc.) ====
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Apmf Do Col Est ", "^Apm Do Colegio Estadual",
                                                                 "^Ass Pais Prof E Func Do Centro Mun", "^App E E ",
                                                                 "^Assoc Pais E Prof C E ", "Circulo De Pais E Mestes Da Escola Estadual",
                                                                 "^Uex Assoc\\. De Pais Mestres Cmef",
                                                                 "^Cons\\. Delib\\. Da Com\\. Escolar Do Cme",
                                                                 "^Cons\\. Delib\\. Da Com\\. Esc\\. Da Eepg",
                                                                 "^Ass Pais Mestres E M ", "Cons\\. Delib\\. Da Com\\. Esc\\. Da Eepsg",
                                                                 "^Cdce Da Escola Municipal", "^Conselho Escolar Do Colegio Estadual",
                                                                 "^Cons Esc Col Estadual", "^C\\. E Do Col\\. Estadual",
                                                                 "Conselho Escolar Do Cpmg", "^A P M Da E E ", "^E\\.Me\\.E\\.I",
                                                                 "Conselho Escolar Do Ginasio Estadual", "^Cons Esc Da E M",
                                                                 "^Uex Eefm", "^Conselho Escolar Da Em", "Cax Escolar Da Escola Mun",
                                                                 "Cons\\.Da Esc\\. Mun\\.", "^Cemeif", "^Conselho Da Esc De Eif",
                                                                 "^Ueemeif", "App Das Esc\\. M\\.", "App Do Colegio Municipal",
                                                                 "App Do Colegio Munic\\.", "App Das Esc. M\\.", "App Do Colegio M\\.",
                                                                 "Assoc\\. De Pais E P\\. Do C\\. M\\.", "Cplegio Munic\\. Dep\\.",
                                                                 "App Da Sc\\. Munic\\.", "U\\. Exec\\. Da Esc\\. M\\.",
                                                                 "Unidae Exec\\. Da Esc\\. Munic\\.", "Unidade  Exec\\. Da Escola Munic\\.",
                                                                 "App Das Esc\\. M\\.", "Uex Assoc Comunid Escol Do Col Munic",
                                                                 "^Esc M Manoel", "^Uex Da Escola Mun", "^Uex Escola Municipal",
                                                                 "^Uex Da Escola Municipal", "^Conselho De Escola Da Eeefm",
                                                                 "Conselho De Escola Da Ep Vila De Jetiba", "^Conselho De Escola Do Cmei",
                                                                 "^Conselho De Escola Da Eeef", "^Apam Da E M",
                                                                 "^Cce E", "^Apm Da Ee ", "^Assoc De Pais Mestres Ee ", 
                                                                 "^Apm Eme ", "Cem Jose Jardim Correia Teteo",
                                                                 "Unidade Executora Da Creche Municipal", "^Unidade Executora"), 
                                                               collapse = "|")), 0, ind_osc))
    
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("Cec Da Cm", "Cec Da Creche Municipal",
                                                                 "Cec Municipal", "Cec Da E\\.M\\.", 
                                                                 "Cec Da E\\. M\\.", "Cec E\\.M\\.",
                                                                 "Cec Em"), collapse = "|")), 0, ind_osc))
    
    ## Remove buildings, more condominiums, and condominium associations =======
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Shopping Center", "^Estancia Cabral",
                                                                 "^Assoc Dos Condominos", "^Assoc Dos Cond Do Edif ", 
                                                                 "^Associacao Do Lot Res ", "^Assoc Cond Edif Resid ",
                                                                 "Assoc Dos Cond Do Residencial", "^Galeria Comercial Di Brunno",
                                                                 "Conjunto Vidal Natividade Da Silva",
                                                                 "^Associacao Condominios  Do Edificio", "San Marino Residencial I",
                                                                 "^C. Edificio Topazio", "^Assoc Dos Cond Do Edificio",
                                                                 "^Cond\\.Civ\\.Pro Ind Shop\\.Center", "^Cond. Residencial",
                                                                 "^Cond\\.Edificio", "^Centro Empresarial", "Centro Clinico Santa Isabel",
                                                                 "^Edificio Resid\\. E Com\\.", "^Cond\\. Edif\\. Resid\\.",
                                                                 "^Comdominio ", "^Ccap Condominio Cuiaba", 
                                                                 "^Comove Condominio Do Residencial", "^Flamingo Gold Residence",
                                                                 "^Associacao  De Condominios", "Assoc Cond E Mor Ed",
                                                                 "^Centro Empresarial", "Cindominio Do Edificio", "^Cond\\. Edif\\.",
                                                                 "^Conjunto Riviera", "Enseada Ocean Frontde Sc",
                                                                 "Green Plaza Shopping", "Alessander Gasparelo", "Caragua Praia Shopping",
                                                                 "Associacao Do Condominio Chacara Maria",
                                                                 "Comercial Podium Center", "Assoc Condominio Canto Da Baleia",
                                                                 "Associacao Dos Condominos Residencial Casablanca",
                                                                 "As Condominio Santo Antao", "^Cond\\.Resid\\.", "^Apart Hotel Sandiego",
                                                                 "^Loteamento Dois Irmaos", "^Cond\\.Edif\\.", 
                                                                 "^Hotese Hoteis De Sergipe Sociedade Anonima", "^Cond\\. Ed\\. Comend\\.",
                                                                 "^Cond\\. Res\\.", "^Shopping Das Confeccoes", "^Edificio ",
                                                                 "^Cond\\. Do Ed\\.", "^Cond\\. Ed\\. Res\\.", "^Empresarial ",
                                                                 "^Residencial ", "^Edificio Resid", "^Condiminio Do Edificio"), 
                                                               collapse = "|")), 0, ind_osc))
    
    ##' Remove corporations and the like =======================================
    ##' (sociedades empresarias, simples, anonimas, empresarios individuais
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("Shopping Exactus De Informatica", "Open Dojo Lab",
                                                                 "^Pini & Paulalda", "Comissao Organizadora Do Iea 2012",
                                                                 "L M Gobira De Souza", "Agro Pernambuco"), 
                                                               collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Isento$", "^Construtora Castro Reis", "C.F. Timm", "Elmo Da Rocha",
                                                                 "Usina Hidreletrica Chapadao", "Ph Servicos Construcoes Eireli",
                                                                 "^Denilson De Almeida", "^Laylanne Silva", "^Comprev Previdencia Sa",
                                                                 "^Sunset Do Brasil Comercio", "^Sarkis Advocacia", "^G Palmas Dos Santos",
                                                                 "Fap Empresa Consultoria E Assessoria", "Iveacre Inspecao Veicular",
                                                                 "^Etivaldo Diniz", "^Gilmar C De", "^Jean Moraes", "^Loren E Magalhaes",
                                                                 "^Vera Maria De", "^Ilsa De Nazare", "Jornal Foco Noticias",
                                                                 "^S P Arruda", "Lopes E Vechmeyer Advocacia S C", 
                                                                 "^Mbm Previdencia Privada", "C S Lima Santos-Me", "^Adevanildo De Lima",
                                                                 "^A P Froes Junior", "Posto Dee Taxi Uniao", "I B Freitas",
                                                                 "Posto De Taxi Sao Jose", "Adeilson P\\. Santos", "^E Da S Matos",
                                                                 "C F Abreu Comercio", "^L A S Lima", "Alline Do S Mendes Rego", 
                                                                 "^Ailton Soares", "Com De Tecidos E Confeccoes", "^F R Ferreira",
                                                                 "^Idelson Pereira", "^Amelia Teresa", "Contabilidade-Me", 
                                                                 "^Alessandra Soares", "^Cosmo Torquato", "^Claudina Barbara",
                                                                 "^Joana Bastos", "^Cicero Romao", "^F Das Chagas", "^Nayara De Fatima",
                                                                 "Torres Pinto Telecon E Representacoes", "^Valeamar De Ceara",
                                                                 "^Hotel Pousada ", "^Gerlania Lins", "^Comercio Varejista De Artigos",
                                                                 "^Green Center$", "^Raimundo Almeida", "M C M  Advogados Assc  S C",
                                                                 "^Bolsa Mercantil", "Pitang Consultoria E Sistemas",
                                                                 "Pellon E Associados Advocacia", "Bolsa De Valores De Pernambuco E Paraiba",
                                                                 "^Wilane Ferreira", "^Vanesa Oliveira", "Advocacia E Consultoria",
                                                                 "^Nilton Cesar", "^Raphael Wagner", "^Lindaura Alves",
                                                                 "Consorcio Tynes/Oms/Sumare", "^Nelma Rodrigues", "V\\. N\\. De Sousa",
                                                                 "^Luzitania De Oliveira", "Camcontec", "Edizia Dos Santos",
                                                                 "^Tecno System", "D\\. De Moura Andrade", "Mercado Central Abastecimento E Servicos",
                                                                 "Mc Minas Contabilidade", "^Marta Da Felicidade", "^Bolsa De Valores Minas",
                                                                 "^Juscileni De Lima Araujo", "A Romana Administracao E Corretagem", 
                                                                 "Carvalho Advog Associado", "Bolsa Brasileira De Mercadorias",
                                                                 "Sirius Chales S/C", "^Dania Rezende Forli", "^Rede Super Farma",
                                                                 "Legal Corporation Eireli", "^Gesiel Carlos", "^Hebert Luiz", 
                                                                 "Pereira Advogados", "Teixeira Advogados Ass", "^Olinda Dos Santos",
                                                                 "^Elcione Rosa Oliveira", "Taxiserv Taxi E Servico De Bh",
                                                                 "Combinados Para Apoio A Edificios", "^Pinheiro E Silva", 
                                                                 "Caetano Materiais De Construcao", "^Wendelle Cesar",
                                                                 "^Gislaine Aparecida", "^Comissao De Formatura ",
                                                                 "Solucoes E Producoes Eirele", "^Vale Do Aco Convention",
                                                                 "Julio Cesar Palheiros", "Jayme Jose Duarte Couto",
                                                                 "Condedifmarcy Deotti Ibrahim", "^Milton Rodrigues",
                                                                 "^Eleuza Dorneles", "^Edielle De Cassia",
                                                                 "Consultiva E Participacoes Eireli",
                                                                 "^Cristina De Calcuta", "Luilson De Freitas-Me",
                                                                 "Ivete Tomazia Da Luz Costa-Me", "Comdec Jequitiba Mg",
                                                                 "^Toec&Vb$", "^Patris Business Club", "^Dem Florestal",
                                                                 "Minas Lacteos Assessoria", "Eduardo Augusto Jardim Advogados",
                                                                 "Cleuzeoni Pereira Da Silva -Me", "^Rosilane Pereira",
                                                                 "^Fabio Pereira", "^Sander Goncalves", "^Servico De Educacao$",
                                                                 "^Edificio Comercial", "^Ivonyr Magna De",
                                                                 "^Leandro Teodoro", "Armario Visual Com E Ind De Moveis",
                                                                 "Uberaba E Regiao Convention & Visitors Bureau",
                                                                 "^W Pawer Ribeiro", "^Humberto Coelho De",
                                                                 "^Joubert Tupi", "Tarcmem Trib Arbitr Conc Med Mercosul",
                                                                 "Transportadora Irmaos Sa", "Terminal Indl. E Multimodal Da Serra",
                                                                 "^Santos Germano De", "^Rede Utilicasa$", "^Juliane Aparecida",
                                                                 "Villa  Rica Motel Country Club", "^Bolsa De Valores",
                                                                 "Associacao Disk Taxi De Realengo",
                                                                 "Copacabana Posto 6 Taxi", "^Centtrum Contact Center",
                                                                 "Com Varej De Merc Gera", "Pm Da Silva Couto Pousada",
                                                                 "Grand Marketing Ass. E Propaganda",
                                                                 "Aravo Consultoria E Contabil", "Fss Apoio Administrativo Eireli",
                                                                 "Seneca Incorporacoes Imobiliarias Sa",
                                                                 "^Rodolfo Jose ", "Imopar Participacoes E Empreendimentos",
                                                                 "Rc E Tn De Casimiro De Abreu", "Cbs Bike Pecas E Acessorios",
                                                                 "Taxi 28 De Setembro", "Devcelly Consultoria Pericial Ld",
                                                                 "Consorcio Dps-De Consult.Praxis Sysfer",
                                                                 "Freire E Vilas Boas Advogados", "^Distribuidora E Armarinho",
                                                                 "^Deposito De Bebidas", "Serra Junior Engenharia S/C", 
                                                                 "^Esplanadas Club$", "Clube De Seguros Cissex", "Cerf Administracao",
                                                                 "Monteiro E Baciega Advogados Associados Sc",
                                                                 "Abel De Paiva Contabilida", "^Ayres Monteiro",
                                                                 "Mendes E Nakamura Advogados", "Bolsa De Cereais De Sao Paulo",
                                                                 "B Melcher Eireli", "Gta Comercio E Servicos De Aparelhos",
                                                                 "Oleoduto Norte", "^Business Software Alliance",
                                                                 "Use Intermunicipal De Jundiai", "Ultra S A Participacoes",
                                                                 "Diadema Assessoria Juridica", "Fernao Dias Logistica Eireli",
                                                                 "Di Mino Manutencao Industrial Sc", 
                                                                 "Bolsa De Mercadorias De Sao Paulo",
                                                                 "^Itaici$", "Caixa Nacional De Liq De Negoc A Termo E Disponivel",
                                                                 "Pubservicos Automotivos Sc", "Forssell Sociedade De Advogados",
                                                                 "Luciane De Lima Transportes", "C De Augusto Lopes",
                                                                 "J Sampaio Sousa Industria E Comercio", "^Dnaiel Belo",
                                                                 "^Macedo E Sampaio", "^Cicero Eudes",
                                                                 "^Tamires Sousa", "^Silas Xavier", "^Orivaldo Angelo",
                                                                 "^Ademir Gomes", "^Walter Alexandre", "Atlantic City World Club",
                                                                 "Construtora Morada Nova", "^Oscar Paula",
                                                                 "Jeova Candido Da Costa-Me", "^Veridiano Da", "^Emilson Carneiro",
                                                                 "Jair Delfino Cardoso", "Edinaldo Bispo De Souza",
                                                                 "Rhepfon Mineracaoind E Comercio Sa", "Miller Felipe Mendes",
                                                                 "Marcenaria Paiva", "^Geni Rodrigues", "^Ubaldo Antonio",
                                                                 "^Edson De Matos", "^Wilson Cardoso", "^Paiva E Filho",
                                                                 "Kleber Goncalves Da Silva", "Amanda Natal Martins",
                                                                 "Jorge L De Miranda", "Acotecnica Sa", "Geerli Vidal De Almeida",
                                                                 "Alicio Rheinheimer", "J\\.De Souza Telas", "Otavio Trindade Lopes Junior",
                                                                 "Aldo Fronza Instalacao-Me", "Alexandre Nunes",
                                                                 "^Cleiton Amadeus", "M L Frozza Instalacoes", "Priscila Palhano Da Silva",
                                                                 "Juarez Da Luz De Lima Cruz", "R R Mercearia",
                                                                 "Elisa Susana Gutierrez", "Alexssander Pereira De Souza",
                                                                 "^Iry Antonio", "A Santini Empreendimentos Eireili",
                                                                 "Rose Mari Matzenbacher-Me", "Doracy Izabel Da Cruz Hubert",
                                                                 "^Alessandra Machado$", "Advogados E Asciados",
                                                                 "^Diego Rafael", "^Augusto Carvalho", "^Eloi Mazur",
                                                                 "^Everton Willian", "^Remi Lanzarin",
                                                                 "Katia Dias Carneiro-Produtos Agropecuarios",
                                                                 "Formatura Puc Di Pp 2006 2010",
                                                                 "Cicarello E Orthmam Contadores Associados",
                                                                 "Fire Informatica", "Mercado 3M De Terra Roxa", 
                                                                 "^Franciele Maria", "^Larroyd Advogados", "^Creoni Borghezan",
                                                                 "^Comprev Previdencia Sa  Fln",
                                                                 "Advogados Assoc", "Advogados Associados",
                                                                 "Oliver Ullmann", "Solano Forguieri Sosnowski",
                                                                 "Hellen Transportes Rodoviarios De Cargas", 
                                                                 "P E G Consultoria E Assessoria", "Bar E Churrascaria Colonial",
                                                                 "Rogeria Cassou", "Rodrigo Peller", "Jacson Arsenio Taufer",
                                                                 "Nycolai Da Silva Oppa", "Juliane Maia Morais",
                                                                 "P1 Agencia De Turismo De Joinville", 
                                                                 "Charles Guglielmi Transportes", "Anito Mezzomo",
                                                                 "^Rei Dos Reis Atacadista", "Bijuteirias E Acessorios",
                                                                 "Gadotti Farah E Ranzi Assessoria Juridica",
                                                                 "Vitalicio Perin", "Younis Corretora De Seguros",
                                                                 "Dorvalino Biolchi", "^Jtc Comercio", "Lothario Dremer",
                                                                 "W Master Video Locadora", "^Zedelina Da", "Reach Consultoria",
                                                                 "Comercio De Cosmeticos", "^Joelson Orrigo", "Advogados Associad$",
                                                                 "^Cristiano Pereira", "Assessoria E Comercio De Creditos",
                                                                 "Deolinda De Oliveira", "Dilson Dall Agnol", "Eraldo Rocha Pereira",
                                                                 "Armindo Carniel", "F B Tassitai Producoes De Vendas", 
                                                                 "Bernardete Angelina Giordani", "Fabricio Pelizzer Teixeira",
                                                                 "Henrique Mayer", "Flori Schoeninger", "Ghisleni E Holanda S/C E Cia-Me",
                                                                 "Csar M Dos Santos", "Erlaine Borges Candor Vargas",
                                                                 "Janes De Rodrigues Rodrigues", "Nelsi Moura", 
                                                                 "^Blue Wave Assess", "Peter Teichrieb",
                                                                 "Valdirana Zago Lopes", "Cbm Representacoes", "^Celeso Da",
                                                                 "Darcio Floriano Lopes", "^Evelyn Bernardes",
                                                                 "^Thandara Faleiros", "^Holsbach Holsbach",
                                                                 "^Adao Roberto", "Advocacia E Consultoria",
                                                                 "^Mm Advocacia", "Advocacia E Assessoria S/C", "^Valdemir Sola",
                                                                 "Poranpecas E Acessorios Para Veiculos", "Kathia Teixeira De Padua",
                                                                 "^Wania Maria", "^Jamiro Soares", "^Lionidia Barbosa",
                                                                 "^Denise De Freitas", "^Derich Rodrigues", "^Weslei Alves",
                                                                 "^Leusa Dos Santos", "Brasilia Country Club"), 
                                                               collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Nevio", "^Carmen Lucia", "^Conceicao", 
                                                                 "^Ruberlandio", "^Arnaldo", "^Adao Ferreira"), collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("-Me$", "-Sa$"), 
                                                               collapse = "|")) &
                                    !str_detect(razao_social, paste(c("Associacao", "Asso ",
                                                                      "Ministerio"), 
                                                                    collapse = "|")), 0, ind_osc))
    
    
    x <-  x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Schahin", "^Empreendimentos",
                                                                 "^Minimercado", "Mcf Modas", "Acessorios Para Veiculos", "^Gilmar", 
                                                                 "^Claudio", "^Sebastiana", "^Adriane", "Tuquinhas Pneus",
                                                                 "^Marcio Koeller", "Water Cooker Industria E Comercio", "^Valdete",
                                                                 "Phonix Publicidades", "Mercado Janbo", "^Ovaldeth",
                                                                 "^Jocinaldo", "^Agenaldo", "^Efigenio", "Armarinho$",
                                                                 "^Flora Alice", "^Irene Marques", "^Damiao Jose", "^Adailton",
                                                                 "^Eufrasio", "Material Hospitalares", "J Vieira Neto", "^Marinaldo",
                                                                 "^Idione", "^Norma", "W G Valente", "^Joaquim", "^Juciene",
                                                                 "^Ezequiel", "^Laelson", "^Mercado", "^Rosanira", "^Oldair",
                                                                 "^Leonir", "Maquinas Textil", "^Maristela", "^Marilda", 
                                                                 "^Laercio", "D Silva Soares", "^Everaldo", "Young Jonn Lee",
                                                                 "Ilma Dantas", "^Luciani", "Para Caminhoes", "^Edmilson",
                                                                 "Tropical Piscina", "^Inez", "^Edelmira", "^Alecio", "H\\. M\\. C\\.",
                                                                 "^Silvana", "^Centralpan", "^Clayrton", "Kenys Bonatti",
                                                                 "M De G De S Magalhaes", "Beloni Vendruscul", "Vidao", 
                                                                 "C  Aires Da Silva", "Comercial Ag Moreira", "Olinda Dos Santos", 
                                                                 "A G Do Rosario", "^Terezinha Conceicao", "^Mauro", "^Marcio", 
                                                                 "Moda Infantil", "^Cond. Vale", "^Adilsn", "^Albino",
                                                                 "^Adminircio", "^Ailton", "M A B Pinheiro", "Grande Marca Outlet",
                                                                 "^Geneci ", "^S\\.Marques", "^F I Soares Santos", "^Higienitech",
                                                                 "^Ailton", "^Karla", "^P F Valbuza", "^Lizete","^I Felix",
                                                                 "^Eliomar", "^Vidracaria Faz Box", "^Representacoes Comerciais",
                                                                 "^Rosilene",  "^Luciane", "^R R  Portela Dias", "^Hildegard Ingeborg",
                                                                 "^R Vasconcelos ", "^Jario ", "^Sport Golden ", "^Monica ",
                                                                 "^Adriano Jose Antonio", "^Pini & Paulalda", "^Mercadinho",
                                                                 "^Leila Cristiane", "^Juvenal ", "^Aecio Flavio ", "^Leticia B ",
                                                                 "Colares E Consul Educacionais", "Costa Mercearia", "Comercio De Combu,stiveis",
                                                                 "Repres De Evento", "Caramelos E Armarinhos", "^Alan Almeida",
                                                                 "D. De Moura", "Amorim Vidracaria", "^Eliezer ", "^Giovanni Willi",
                                                                 "J V S Almeida", "^Dicilene ", "^Franciele ", "^V.Lucia ","Mercado 3M",
                                                                 "J Souza Cruz", "Bijouterias", "Dos Santos Comercio",
                                                                 "F Do S B D Silva", "^Marilene ", "^Elizete ", "F G Rodrigues",
                                                                 "^Lazaro ", "^Severino A De ", "^Jonathan ", "^Celia ","^Edvaldo ",
                                                                 "^Claudio ", "^Aparecido ", "^Janaina ", "^Franco ", "^Elandio ",
                                                                 "M A F Rodrigues", "A Rocha Da Costa",  "Importacao E Exportacao", "^Kehinde ",
                                                                 "L.F.Da Silva ", "R.C.Mesquita ", "M V A Andrade", "M V Moraes Oliveira","^Meriane ",
                                                                 "^Mauro ", "^Jandira Silva", "^V S Fernandes ", "^M P F Dos Santos", "^Miranda",
                                                                 "^Roges ", "E Da S Magalhaes", "^K N Moraes ", "^E M De Souza", "^M I Farias ",
                                                                 "^Victor J.C.Vidal", "Pq Fernades", "^Raimundo ", "^M J Siqueira ", "^Nathalia ",
                                                                 "^Bibiana Sidartha ", "Celular Telecom E Informatica", "^P Henrique",
                                                                 "^Celio ", "^Simon ", "^W Santos", "^Edinalva", "^Ricardo ", "^Iara Maria", 
                                                                 "^Catarina ",  "Gallium Latin", "^Gilvane ", "Jural Combustiveis", "^Rogerio ",
                                                                 "C B De Souza", "^Araujo ", "^Mauro De ", "^Samuel ", "^Jorge Trindade",
                                                                 "^M Angelica ", "^Aurenizia ",  "^H J Dos Santos", "^Andre Cedro",
                                                                 "^Edvando ", "^M Dalva ", "Blog Informatica", "^Helen",
                                                                 "Moura Confeccoes", "A E C B  Cavalcanti", "^Vamir", "^Orlamar",
                                                                 "Nmss Materiais Para Construcao", "^Andre Conde ", "^Stephanie",
                                                                 "Roupas E Acessorios",  "^Erlizandra ", "^Evandro ",
                                                                 "^Lucio Antonio",  "Carlos Personalizacoes", "^Josileno ",
                                                                 "^R\\. Da Silva", "^Melissa ",  "Leiticia Cosmeticos",
                                                                 "Souza Artesanatos",  "Comercio De Aparelhos", "W R S Sette",
                                                                 "W.C.Kincke",  "Comercio De Alimentos", "J. Alcantara Moreira",
                                                                 "Comercio E Representacao", "^Vera L ",  "^Sonia",
                                                                 "^Nuirio ", "Silva Cosmeticos", "^Angela Maria", "L.C.Carvalho", 
                                                                 "Thaizi", "^Thiago De", "Franco Representacoes", 
                                                                 "Suprimentos E Informatica",  "^Laercio Camargo", "^Arguelho",
                                                                 "^Andre Luiz", "^Gilvan ", "^Cristiane", "Papelaria Informatica", 
                                                                 "^Wander ", "^Edmar ", "^Edson Pereira",  "^Sonia ", "Farmacia Popular Do Brasil",
                                                                 "^Otavio", "^Neusa", "Issam Kamel", "R C Girundi", "^Wagner Araujo",
                                                                 "Rodrigues Importador",  "Associacao Dos Idosos De Lins - Me", "Gildete Lima", "Braz Pereira",
                                                                 "^Claudio ", "^Albert Zoll", "^Joaquim Carlos" , "Cial.Agromen Maquina",
                                                                 "Masut Combustiveis",  "Diva De Alencar","Ester Pooter", "^Liliam ",
                                                                 "^Edna Maria", "S M Moreira ", "^Martha ", "^Germana ", "^Hugo De ",
                                                                 "^Everton ", "^Euclides ", "Trading", "Expor\\.S/A",
                                                                 "^Terezinha", "^Dilson", "^Erna", "^Demetrius", "^Rosilene", 
                                                                 "^Milton ", "^Carmena", "^M Rocha", "^Ciomar", 
                                                                 "^Selma ", "^Arnaldo", "S.T.I. Papel De Jagva - Farmacia", "^Nelso", 
                                                                 "^Otavio", "^Vitalino", "^Jairo", "Hipermercado", 
                                                                 "Comercial Elo", "^Helmar", "^Juarez", "Atacadista Textil", "Drogaria E Farmacia",
                                                                 "Mercadinho E Casa", "Laramara Otica"), collapse = "|")), 0, ind_osc))
    
    x <-  x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Orlando Bucco", "Jair De Orlenas",
                                                                 "Jorge", "^Arlindo",
                                                                 "Jair", "^Valdir","^M R Construcao",
                                                                 "^Habitecnica Sa Empreend", "^Obra Renato Bergamo",
                                                                 "^Obra Ramiro Pires","^Clarkkson",
                                                                 "^Djalma","^Francivaldo",
                                                                 "^Celio", "^J A Santana",
                                                                 "^Rcl Comercio E Servico De Emprendimento",
                                                                 "^Schahin Empreendimentos Imobiliarios Spe 1 Sa",
                                                                 "^Schahin Capital Spe1 Sa","^Fabio Willian Trindade",
                                                                 "^Lucimar","^Natalino",
                                                                 "^Rj Construcao E Servicos Imobiliarios",
                                                                 "^Norberto",
                                                                 "^Residencial Dona Lazara",
                                                                 "^Construtora Terrestre Limitada",
                                                                 "^Cesar", "^Edificio Canaa", 
                                                                 "^Cond\\.",
                                                                 "^Erika",
                                                                 "Obra Do Condominio Residencial Alto Dos Pinheiros"), collapse = "|")), 0, ind_osc))
    
    x <-  x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Raimundo",
                                                                 "^Miguel", "^Residensial",
                                                                 "^Vieira E Fraga Intermediacao De Negocios",
                                                                 "^Schahin Energia Sa",
                                                                 "^Iucam Instituto Universitario Candido Mendes Sc",
                                                                 "^Mpx Energia Sa", "^Companhia Schahin Securitizadora De Creditos Finance",
                                                                 "^Nantesprev Corretora De Seguros De Vida Limitada",
                                                                 "^Companhia Schahin De Ativos", "^Schahin Ativos Companhia Securitizadora De Creditos",
                                                                 "^Comdominio Edifio Perola", "^S S Holding Eletrica Sa",
                                                                 "^Ical Participacoes Sa", "^Rede Jardim Itamarati Empreendimentos Imobiliarios",
                                                                 "^Acs- Administracao De Shopping Cente",
                                                                 "^Everson Souza Rosa", "^Everton Francisco Da Rosa",
                                                                 "^Residencial Positano", "^Orlando Jose Da Silva Machado",
                                                                 "^Comdominio", "^Cond", "^Cond\\.", "^Residencias", "^Condominio",
                                                                 "^E S Fonseca", "^Residencial Cezanne",
                                                                 "^Carta Matrix", "^Comdominio Rosa Cecilia",
                                                                 "^Jb Empreendimentos E Participacoes",
                                                                 "^Condonimo Do Edificio Grafitti",
                                                                 "^Residencial Sao Paulo",
                                                                 "^Companhia Schahin De Ativos",
                                                                 "^Residencial Santa Helena", "^Emb Da Rep Bolivar Da Venezuela No Brasil"), collapse = "|")), 0, ind_osc))
    
    x <-  x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Departamento Municipal",
                                                                 "^Internacional Federation Of Social Workers",
                                                                 "^Rede De Informacao Tec. Latino Americana",
                                                                 "^Instituto Interamericano Pesqmudancas Globais",
                                                                 "^Schahin Securitizadora De Creditos Financeiros Sa",
                                                                 "^Habitecnica Participacoes Sa",
                                                                 "^Quality Lab Participacoes Sa",
                                                                 "^Argus Corretora De Seguros",
                                                                 "^Itapemasa Itaqui Petroleo E Maquinas Sa",
                                                                 "^Central De Compras Assorverplan",
                                                                 "^Gino De Biasi", "^Raimundo",
                                                                 "^Luvanor", "^Miguel",
                                                                 "^Gleison", "^Leandro", "^Leonardo", "^Francisco", 
                                                                 "^Wagner", "^Liziane", "^Lidiane", "^Lisandro", "^Janede",
                                                                 "^Francicleide", "^Izilda", "^Mirtes",
                                                                 "^Josias", "^Andre", "^Crisogono", "^Guilherme", "^Paula",
                                                                 "^Diego", "^Joaquim", "^Madeireira", "^Elisangela",
                                                                 "^Adailton", "^Abiner", "^Reginldo", "^Lucilvanie", "^Luana",
                                                                 "^Gilmar", "^Euripedes", "^Wellinson", "^Neila", "^Fatima",
                                                                 "^Edinei Consolin", "^Rm Auto Lavagem", "^Edcarlos", "^Silverio",
                                                                 "^R P Cabrera Chapeacao", "^Acumuladores Moura Sa", "^Germano", "S A Schirmer Cosmeticos",
                                                                 "^R A Mecanica De Automoveis Lda", "^Erica", "^J Fernandes De Oliveira", "^Bartolomeu", 
                                                                 "^Shopping Do Automovel Do Rio Grande Do", "^Rosiana", "^G Borges Da Silva",
                                                                 "^F Dos Santos Guaitolini", "^C Da Silva", "^ S P Machado", "^P S N Pinto", "^Supermercado",
                                                                 "^Aurea", "^Rubi Porch", "^C B De Oliveira", "^Mercado", "^J H Rodrigues Relojoaria", 
                                                                 "^V S Feliciano", "^Slr Veiculos", "^S S Fiuza Pecas",
                                                                 "^ Mirian Ventura Da Silva", "^Vanguarda", "^Oficina De Todos Os Santos",
                                                                 "^M Turazzi", "^Alcino", "^Marcio", "^Mara", "^Artu", "^Arthur", "^Artur",
                                                                 "^Marcio", "^Slr Veiculos", "^V S Feliciano", "^Barbara", "^Elaine", "^Renato",
                                                                 "^C B De Oliveira", "^Poranpecas E Acessorios", "^Eliana", "^Eliane", "^ Maibi Confeccoes Calcados",
                                                                 "^Embapack Representacoes Comerciais", "^Editora", "^Louvani", "^Flora", "Carley", "^Cleide",
                                                                 "^Lafarge Brasil Sa Uberaba", "^Zandomenico E Nascimento", "^Celeste", "^Dulce", "^Lisangela",
                                                                 "^Martha", "^Helio", "^Agamenon", "^Iara", "^Benedito", "^Rosangela", "^Ersival", "^Angela", 
                                                                 "^Joniza", "^Edson", "^Mario", "^Fabiana", "^Keila", "^Geralda", "^Nilza", "^Leoni", "^Silvania",
                                                                 "^Getulio", "^Marlis", "^Alexandre", "^Zaqueu", "^Janeci", "^Joilma", "^Rubens", "^Janio",
                                                                 "^Everton", "^Dardara", "^Ester", "^Telmo", "^Telma", "^Ariosvaldo", "^Cesar", "^Ogrimar",
                                                                 "^Arlindo", "^R Vasconcelos De Souza", "^I Dos Santos", "^Abrasco", "^Edimar", "^Nardina",
                                                                 "^Celso", "^F I Soares Santos", "^Theo", "^Saulo", "^Lucilio", 
                                                                 "^Marli", "^O P Moura Calcado", "^Elenice", "^Leydmar", "^Alcinei",
                                                                 "^Minimercado", "^Olivio", "^Daiane", "^Cassionildo", "^Vera", 
                                                                 "^Amilcar", "^Adao", "^Ederson", "^Lilian", "^Lilia", "^Janilza",
                                                                 "^Ruth H M Pereira-Me", "^Ovaldeth", "^Gleyce", "^Marta", "^Alexandre",
                                                                 "^Lourival Almeida Da Silva","^Gilceu", "^Janilza", "^Joel", "^Claudeci",
                                                                 "^Gilceu", "^Valdete", "^Pablo", "^Sirlene", "^Kelen", "^Priscila", "^Adriano",
                                                                 "^Hofstetter", "^Loja Alvorada", "^Claudeci", "^Adeni", "^Rita", "^Divo",
                                                                 "^Irene", "^Suzana", "^Rosania", "^Lidya", "^Fabio", "^Juscileni", "^Luzineide",
                                                                 "^Lourival", "^Seiji Nakumo", "^Raimundo", "^Iolanda", "^Laercio",
                                                                 "^Vanda Santos Da Silva", "^Mauro Chagas Da", "^Arizete Costa Do Carmo",
                                                                 "^A.Luciano Luizi-Me","^M Margarida Pinheiro Da Silva", "^Joilson",
                                                                 "^Fernando", "^Thiago", "^Otacilio", "^Suelen", "^R K Soares Confeccoes",
                                                                 "^P N Da Silva Generos Alimenticios", "^Samuel", "^Andre",
                                                                 "^Aureniza", "^Suely", "^Gilson", "^Celio", "^Elano", "^Ygor",
                                                                 "^Dorival", "^Sindicato", "^Angelo", "^J Borges Pinto", "^Zema Eventos",
                                                                 "^Mma Industria E Comercio", "^Mjb Industria E Comercio", "^Mro Industria E Comercio",
                                                                 "^Mbe Industria E Comercio", "^Mmc Industria E Comercio",
                                                                 "^ Emplastec Industria E Comercio", "^Fit-Industria E Comercio", 
                                                                 "^Bem Atenta Industria", "^Amazonia Explosivos Industria", 
                                                                 "^Industria De Azulejos Sa", "^Industria De Azulejos Sa",
                                                                 "^N Arduim", "^S A Fabrica De Prod Alim Vigor",
                                                                 "^Ivonete", "^Valdevino", "^Rosana", "^E Dornelles",
                                                                 "^F Da Silveira Gato", "^Daniel", "^N V Mesquita Panificadora",
                                                                 "^Cia Alagoana De Refrigerante", "^Djalma", "^Acao Total",
                                                                 "^Salao de Beleza", "^Otica Fiel", "^Cardoso Otica", "^Solace Engenharia",
                                                                 "^Otica Symar", "^Otica Pampulia", "^Otica Fiel", "^Cardoso Otica",
                                                                 "^Laramara Otica", "^J.De Souza Telas", "^Francinildo", "^Leila",
                                                                 "^Lidia", "^Feiticeiira", "^Eulina", "^Trasportes Rodoviarios",
                                                                 "^Adalton", "^P G Chaves Confeccoes", "^Idenir", "^Delcy", 
                                                                 "^Edilma", "^ R.V. Ramiro Industria De Confec", "^A M Azoppi Confeccoes",
                                                                 "^J P De Souza Representacoes", "^Ivone", "^Ernesto",
                                                                 "^A C F De Souza Microempresa", "^Comad", "^Jaime",
                                                                 "^Moretson", "^Anibal", "^Irisnado", "^Faustino",
                                                                 "^Jhonata", "^Divino Moreira", "^Soely",
                                                                 "^A Carla Cristina", "^Leticia", "^Jaime", "^Pedrelino",
                                                                 "^Marinalva", "^Nissan", "^Aderaldo", "^Alaides",
                                                                 "^A L Da Silva Reis", "^ Ml Bastos Siqueira",
                                                                 "^Dario", "^Anderson", "^Divina Silva", 
                                                                 "^Gehan Clever", "^Tigar Ind", "^Alessandro", "^Ildeu",
                                                                 "^Sirleno", "^Gladys", "^Hermann", "^Moacyr",
                                                                 "^Frederico", "^Hamilton", "^Ilsa", "^Maurilio",
                                                                 "^A L Silva", "^Miriam", "^I M Da Silva De Lima",
                                                                 "^M Ferreira Da Silva Comercio", "^I Dias Santos",
                                                                 "^Adamo Ag", "^Denise", "^R A L Moreira",
                                                                 "^D D Arrais", "^Jane", "M\\. A\\. De Sa", "^M L Ruteski",
                                                                 "^Francilda", "^A. R. De Paiva Filho", "^Ademir",
                                                                 "^I Dos Reis", "^J Q De Jesus", "^C E Barros", 
                                                                 "^S P Machado", "^L F De Oliveira Rodrigues",
                                                                 "^ P.L.Piquet", "^J Engel", "^E Perreira Paz", "^Remi",
                                                                 "^Gildazio", "^Walmor", "^Walter", "^Inacio", "^Elmo",
                                                                 "^Osmario", "^Silvane", "^Salmao Moutinho", "^Washington",
                                                                 "^M\\. P\\. Rojas", "^Schenato", "^Ari", "^Aloisio",
                                                                 "^Renomir", "^Giancarlo", "^Expedito", "^Giardino",
                                                                 "^Egidio", "^Elisa", "^Laisse", "^Tatine", "^Jonas",
                                                                 "^Silvia", "^Cristiano", "^Luzileide",
                                                                 "^Ione", "^Denise", "^Angelica", "^Isolde", "^Elizeu",
                                                                 "^Restaurante Cantinho", "^Sara", "^R M C De Souza",
                                                                 "^Almir", "^Valdeci", "^J L Uchoa", "^Flori", "^Raquel",
                                                                 "^Adalberto", "^M S Pacheco", "^F Suave Comercial", 
                                                                 "^Emporio do Camarao", "^Barteco", "^Barra Beach", "^Thames",
                                                                 "^Churrascaria e Bar", "^A C C Alves", "^Elias", "^J I Azevedo",
                                                                 "^Trevi Produtos", "^Rodrigues Design", "^Cleusa",
                                                                 "^Epoca Tecnologia", "^Business Software",
                                                                 "^Meantime Desenv", "^Jase Imobiliaria", "^J S Rosa",
                                                                 "^J S Rosa", "^Ventura Empreendimentos",
                                                                 "Advogados Associados", "Adv Ass", "Advog Associados",
                                                                 "Sociedade De Advogados", "^Guimaraes Oliveira",
                                                                 "^Vilma", "^Raul", "^Nicolau", "^Daiana", "^Assis Consultoria",
                                                                 "^Escritorio Contabil", "^Castejon", "^J S Queiroz", "Adv e Assess",
                                                                 "^Cleucio", "^Acao Contabilidade", "^Forsell", "^Tamir Maluf", "^W Loubak",
                                                                 "^Freire e Vilas", "^Pellon e Associados", "^Braz Soares", "^J Freitas",
                                                                 "^Mascarenhas Adv", "Advogados Associad", "^Joubert", "^Longo Advocacia",
                                                                 "^Mello Mazzini", "Advogados Assoc", "^Dantas Duarte", 
                                                                 "^Contaseg", "^Abel", "^Carole e Ticiana", "^Neiva", "^Escritoria De Contabilidade",
                                                                 "^Suetonio", "^Laudemir", "^Campos Chagas", "^Laudemir", "Advogados Associado",
                                                                 "Advogados Assc", "^Faceca", "^Larroyd", "^Serventia Registral",
                                                                 "^Nicolau", "^Software As", "^Conecta Teleinformatica",
                                                                 "^Jcc", "^Sr Cinema", "^A Z Da Mota", "^O Silva Lanchonete",
                                                                 "^Goes", "^Central Park", "^Rest Abai",
                                                                 "^N Kestring", "^Bock Bock", "^Agencia Ita Brasil",
                                                                 "^Garagem Sao", "^A C Maria", "^S\\. O\\. S\\. Celulares",
                                                                 "G\\. T\\. S\\. Gracioli", "^Oficina Sao"), collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Mendes", "^Comercio De Cereais",
                                                                 "^Nova Eletronica", "^Supermercado", "^Park Place", "^Comercial Fz",
                                                                 "^Comercial Reis", "^Comercial Marivam", "^Comercial Nacional Dist",
                                                                 "^Dil Nel", "^Cr Artefatos", "^V.F.Moura Barbearia", "^Rhepfon",
                                                                 "^Auto Pecas", "^Frigorifico", "^Ideal Lat", "^Ind\\. E Com\\.",
                                                                 "^Vicunha", "^Limppano", "^Sifrank", "^Farias Carvalho",
                                                                 "^Decore e Design", "^Log e Print", "^Vr de Amorim", "Cosmeticos",
                                                                 "^Renovadora Pneu", "^Goodyear" , "^Cbc", "^Nisibra",
                                                                 "^Movelnovo", "^J Sampaio", "^Marcenaria", "^Daca Sao Jose", 
                                                                 "^Ehs Do Nascimento", "^Vortice", "^Fluidtec", "^Athos Manutencao", 
                                                                 "^Foco Servicos", "^Lince Engenharia", "^Concreta Consultoria",
                                                                 "^Aimore Participacoes", "^Warehouse", "^Fgr Construtora", 
                                                                 "^Consorcio Cemusa", "^Construtora", "^Tce", "^Gsm Empreiteira",
                                                                 "^P H Servicos", "^M L Frozza", "^Mais Arquitetos", "M\\. Haase", 
                                                                 "^Afl Empreiteira", "^R\\. Y\\. Sakai", "^Posto de Lavagem", 
                                                                 "^A\\. N De Oliveira", "^Pp de Sa", "^I O Do N", "^Renne", "Lava Jato",
                                                                 "^Posto de Molas", "^Charada Comercio", "^Parana Repres", "^E C Dos Santos",
                                                                 "^Ttl Representacoes", "^Tb Freitas", "^G O Comercio", "^J F Coelho",
                                                                 "^Cereias Sao", "^Biosat", "^Realve", "^A4 Comercio", "^Glicomaster",
                                                                 "^Arouca Empreendimentos", "^Metal Monte", "^Navezon", "^Sunset",
                                                                 "A P Ferreira", "^Solampadas", "^Distrimax", "^Racsoft", "J E V Chagas",
                                                                 "^R N Vieira", "Z E Com Varejista", "H L De Carvalho", "Minimercado",
                                                                 "Representacoes", "^Zcm Cons Em Informatica", "^I S Aguiar Informatica",
                                                                 "^ Consulti Consul. Em Tec. De Informatica", "^Silveira Informatica",
                                                                 "^Digirati Informatica, Servicos", "^Shopping Exactus",
                                                                 "^Fire Informatica", "^Lavanderia", "^Jet Print", "Mercearia", "Bazar", "Comercio",
                                                                 "Transportes", "Transporte", "Veiculos", "Transportadora", "Navegacao", "Estacionamento",
                                                                 "Refeicoes", "Lanchonete", "Churrascaria", "Pizzaria", "Security",
                                                                 "Empresa", "Robotica", "^Pc", "Intera", "Restaurante", "^Hotel Pousada Costa",
                                                                 "Holding", "Participacoes", "Dekasseguis", "Textilia", "^Aen Smolka",
                                                                 "^Centauro Vida", "^Habitat S A", "Imobiliaria", "^ Ramos,Okawa", "Business",
                                                                 "Residence", "^Resbonsability", "Locacao", "Turismo", "^Bonseg Vigilancia",
                                                                 "^Conjunto Riviera", "Shopping Center", "^Tracbel Sa", "^Info Robes",
                                                                 "^Key Man", "^Ravian MArketing", "^S E A Montagem", "^Oliveira E Alencar",
                                                                 "J R Camara", "Motel", "Frigorifico", "^Rua Delta Holanda", "^Cd Down",
                                                                 "^Prelazia De Balsas"), collapse = "|"))& 
                                    !str_detect(razao_social, paste(c("Asso", "Associacao", "Org","Clube","Humanas","Advogados",
                                                                      "Fundacao", "Unid", "Congregacao", "Ass", "Ass\\.", "Camara",  "Assocdos",
                                                                      "Escola", "Instituto", "Uniao", "Assoc", "As",
                                                                      "Associacaodos", "Assoc\\.", "Centro", "Nucleo",
                                                                      "Mitra", "Club"), collapse = "|")),
                                0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Deny", "^Carleone", "^Edileusa", "^Cyrne", "^Elcy", "^Simara", "Nirce",
                                                                 "^Julia", "^Rezende e Nascimento", "^Micela", "^Adolfo", "^Sebastiao",
                                                                 "^Marly", "^Vicente", "^Lorete", "^Karita", "^Dalmir", "^Eunice", "^Amara",
                                                                 "^J Carlo", "^Leidiane", "^Jomara", "^Laylane", "^Armando", "^Deivide",
                                                                 "^Gersino", "^Luan", "^Luzinete", "^Alda", "^Patricia",
                                                                 "^C M Da Silva", "^Patricio", "^Nevio", "^Renan", "^Edinaldo",
                                                                 "^Noedir", "^Alan", "^Nilton", "^Tomas", "^Wilson", "^Diana", "^J Samuel",
                                                                 "^Nascimento Servicos", "^Volmir", "^Jjoao", "^J R B Silva",
                                                                 "^Robson", "^Ridlave", "^Alcimar", "^Edwaldo", "^H Q R Dos Santos", 
                                                                 "^Weslin", "^Fermino", "^A A Daltro", "^Dielton", "^Willian", "^Isair",
                                                                 "^Elio", "^Valmor", "^Indioclei", "Nerivaldo", "^Fabricio", "^Luilson", "^Edgar",
                                                                 "^Marjorie", "^Szimanski", "^Ines", "^Larissa", "^A\\. R\\. De Lima",
                                                                 "^T\\. Gomes", "^Benfato", "^A Miguel", "^Eder", "^Claudete", "^Bruna",
                                                                 "^J D De Carvalho", "^Jurandi", "^E C De Abreu",  "G A De V",
                                                                 "^Adijevando", "^Gilberto", "^Ramilda", "^Liordino", "^Vivaldo",
                                                                 "^Julio", "^Ivanilde", "^Albert", "^S Almeida", "^Hebert", "^Clendima",
                                                                 "^Jeane", "^Clecio", "^Vania", "^Rosivania", "^Joabe", "^M J De Melo",
                                                                 "^M Severo", "^Moura e Silva", "^Espedito", "^Magna", "^A K Da Silva",
                                                                 "M V Da Silva", "^Nayara", "^F B Patricio", "^Iana", "^Cristania", "^Paixao Gomes",
                                                                 "^D Souza Dias", "^Katia", "^Nilo", "^Julieta", "^Luzitania",
                                                                 "^Moura e Silva", "^D G Chaves", "^F Das C", "^I F Ribeiro", "^M De N Teixeira",
                                                                 "^Cicero", "^I T Passos", "^M Da Graca", "^C S Lima", "^M D P", "^Rahimy", 
                                                                 "^Myllena", "^A C Alves", "^Alaide", "^D M M Da Silva", "^M A A De Sousa",
                                                                 "^A V dos Santos", "^D J De Bastos", "^Valmir", "^Joziane", "^D R S De Araujo",
                                                                 "^M J De Lima", "^R Luiz Rosa", "^Elder", "^Marizete", "^W S Reis", "^Estela",
                                                                 "^E C Pina", "^Y Pereira", "^Valdeli", "^Eliete", "^J M Cardoso",
                                                                 "^Lindolfo", "^Autimio", "^Rossi Ramos", "^F F De Araujo", "^Marisa",
                                                                 "^Carmen", "^Sidmar", "^Romerito", "^Jose", "^Matheus",
                                                                 "^Januaria", "^Edna", "^Otoniel", "^Dania", "^Juscilene", "^Ivonyr", "^Renata",
                                                                 "^Francielle", "^A V Dos Santos Lima", "^Nahim", "^C De Macedo", "^Eraldo",
                                                                 "^M L Santos", "^Frencielle", "^William", "^Iveti", "^Rosilane", "^Adelino",
                                                                 "^Eleuza", "^Jesuino", "^Marlene", "^Edielle", "^Oscar", "^Vilcondes",
                                                                 "^Flavio", "^Edelson", "^Lazara", "^Mabio", "^Tiba", "^Fernandes", "^Valdemir",
                                                                 "^Oscar", "^Averiano", "^Said", "^Jovino", "^Luzia", "^Laerte", "^Nilda",
                                                                 "^Cidiane", "^Rejane", "^Alvaro", "^Sandro", "^Alice", "^Dioni", "^Beatriz",
                                                                 "^Quellen", "^Loiva", "^Ladir", "^Alex", "^Dinara", "^Gilcione", "^Zilma",
                                                                 "^Osvindo", "^Ivo", "^Elson", "^Doralino", "^Natalina", "^Roselaine", 
                                                                 "^Salete", "^Odete", "^Valdoir", "^Idalci", "^Rui", "^Elizandro", "^Vinicio",
                                                                 "^Pedrinho", "^Paulinho", "^Rudimar", "^Alexssander", "^Brenna", "^Georgia",
                                                                 "^Romano", "^Leide", "^Lizabete", "^Euza", "^Erly", "^Judson", "^Victor",
                                                                 "^Edilza", "^Levy", "^Alessandra", "^Alessander", "^Wilian", "^Benedita",
                                                                 "^Bras", "^Lino", "^Jailton", "^Francielei", "^Maryellen", "^Tatiane",
                                                                 "^Rogeria", "^Severino", "^Onezio", "^Christina", "^Dihego", "^Fabiane",
                                                                 "^Rene", "^Martinela", "^Wanderlei", "^Claudencio", "^Anisio", "^Mauri",
                                                                 "^Otho", "^Irema", "^Jayme", "^Otto", "^Antelki", "^Wallane", "^Alline",
                                                                 "^L R Do Carmo", "^Mailza", "^Roque", "^Dorvalino", "^Lothario", "^Gislaine",
                                                                 "^Arnilda", "^Agostinho", "^Aubisvpg", "^Marilucia", "^Kelly", "^Rado",
                                                                 "^Eneida", "^Mayara", "^Diogo", "^Josuel", "^Edilson", "^Elis", "^Gean",
                                                                 "^Marluce", "^Itamar", "^Josildo", "^Fagner", "^Addler", "^Ronaldo", 
                                                                 "^Gustavo", "^Valter", "^Orlando", "^Jnc Gestao", "^Eloi", "^Kaluana",
                                                                 "^Agnaldo", "^A L Gomes Martins", "^Veronica", "^Ignez", "^Genivaldo",
                                                                 "^Kevele", "^Jader", "^Cleiton", "^Abercio", "^Matilde", "^R Pedro De",
                                                                 "^Odila", "^Kaue", "^Jander","^Wellington", "^Mirella", "^Hercules",
                                                                 "^Souza Manutencoes", "^Margarete", "^Argemiro"), collapse = "|")) & 
                                    !str_detect(razao_social, paste(c("Asso","Org","Clube","Humanas","Advogados"), collapse = "|")),
                                0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("Carajas Consultoria", "^Gilka E Lucas Consultoria",
                                                                 "^Equacao Consultoria", "^Ag Dos Santos Consultoria",
                                                                 "^Cst Consultoria De", "^Best Consultoria",
                                                                 "^Itaplan Campinas", "^Catejut - Consultoria",
                                                                 "^Duxcon Sankato", "L\\.Planas De Meira Mattos",
                                                                 "^Bueno Candiani Consultoria", "^Congesti Consultoria",
                                                                 "Erm Solution Consultoria", "^Athenas Projetos,",
                                                                 "^Inove Consultoria", "^Pau Terra Consultoria",
                                                                 "^Maya Assessoria", "^Pronto Acao Dall", "Giuseppe Dattilio"), collapse = "|")), 0, ind_osc))
    
    
    # Remove some suspended OSCs ===============================================
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, 
                                           paste(c("Advogados Mdb", "Dab Desordem Dos Advogados Da B", "^Inovar - Assessoria"), 
                                                 collapse = "|")), 0, ind_osc))
    
    
    ## Remove public consortia =================================================
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Consorcio Intermunicipal Do Extremo Noroeste De Sao",
                                                                 "^Consorcio Intermunicipal Ribeirao Lajeado",
                                                                 "Consorcio Intermunicipal De Obras",
                                                                 "Consorcio Intermunicipal Vale Do Teles P",
                                                                 "Consorcio Interm Dos Mun De Nv Ibi",
                                                                 "Consorcio Intermunicipal Das Nascentes",
                                                                 "Consorcio Interm Infra Estr  Urb Rural",
                                                                 "Consorcio Intermunicipal Fecula Sul",
                                                                 "Consorcio Intermunicipal De Saude Dos Campos Gerais",
                                                                 "Consorcio Interm Melhor Estrada",
                                                                 "Consorcio Intermunicipal Novos Caminhos",
                                                                 "^Consorcio Intermunicipal De Saude$",
                                                                 "Consorcio Intermunicipal De Saude Da Alta Paulista",
                                                                 "Consorcio Intermunicipal Na Area De Saneamento Amb",
                                                                 "Consorcio Intermunicipal De Saude Da Regiao Serran",
                                                                 "Consorcio Intermunicipal De Saude Da Micro-Regiao Do",
                                                                 "Consorcio Intermunicipal De Saude Entre Vales Do Muc"), 
                                                               collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(cnae_2_0_classe, paste(c("^AR", "^AL", "^EN"), collapse = "|")), 0, ind_osc))
    
    x <- x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("^Consulado Honorario", "Tourist Authority"), collapse = "|")), 0, ind_osc))
    
    x <-  x %>%
        mutate(ind_osc = ifelse(str_detect(razao_social, paste(c("Coord Reg De Des", "Coord Regional De Des",
                                                                 "^Consorcio Intermuncipal", "^Consorcio Intermun",
                                                                 "^Servico De Edu", "^Fundo De Aposentadoria",
                                                                 "^Orlando Bucco"), collapse = "|")), 0, ind_osc))
    
return(x)
    }