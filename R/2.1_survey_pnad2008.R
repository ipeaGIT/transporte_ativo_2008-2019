# Load libraries -----

# https://www.ibge.gov.br/estatisticas/sociais/educacao/9127-pesquisa-nacional-por-amostra-de-domicilios.html?edicao=18338&t=microdados
rm(list=ls())
gc(reset=TRUE)
library(PNADcIBGE) # devtools::install_github("Gabriel-Assuncao/PNADcIBGE")
library(survey)
library(srvyr)
library(data.table)
library(microdadosBrasil) # devtools::install_github("lucasmation/microdadosBrasil")

############## PNAD Create survey design  -----------

#load data set
pnad2008 <- readr::read_rds("../../data/transporte_ativo_2008-2019/pnad2008.rds")

#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu 

# V0102 - Número de controle
# V0103 - Número de série
# V4607 - Probabilidade do setor
# V4608 - Intervalo de seleção do domicílio
# V4609 - Projeção de população 
# V4610 - Inverso da fração
# V4611 - Peso do domicílio
# UPA		Delimitação do município
# V4617		STRAT - Identificação de estrato de município auto-representativo e não auto-representativo
# V4618		PSU - Unidade primária de amostragem
# V4619		Fator de subamostragem
# V4729		Peso da pessoa


# There should be no Missings (NA) in Design Variables
# Count  missing values (NAs)

anyNA(pnad2008$V4729)                # TRUE
sum(is.na(pnad2008$V4729))           # 0
sum(is.na(pnad2008$V0029))           # 0
sum(is.na(pnad2008$V4611))           # 0
length(which(pnad2008$M001 == 1))    # 0


# Subset PNAD with individuals who answered the detailed questionnaire only
# # This eliminates observations with missing values in the weight variable
# 
# pnad2008Det <- data.table::copy(pnad2008)[!is.na(V0029) & M001 == 1]


# Cria objeto de desenho da amostra                      
sample.pnad08 <- survey::svydesign(data = pnad2008,
                           id = ~v4618, # PSU
                           strata = ~v4617, #Strat
                           weights = ~v4729 , #person weight # ? pre_wgt ?v4729
                           nest = TRUE)

# postStratify pnad2008 (Djalma suggestion)
# v4609 => projecao da populacao
post.pop <- data.frame(v4609 = as.character(pnad2008$v4609)
                       , Freq = as.numeric(pnad2008$v4609))
post.pop <- unique(post.pop)
sample.pos <- survey::postStratify(design=sample.pnad08
                                   , strata=~v4609
                                   , population=post.pop)
# 
# # Subset Sample of survey design for aged above 14 years old
# sample.pnad08.14y <- subset(sample.pos, v8005>=14)

# 3) Save survey ------
saveRDS(object = sample.pos
        ,file = "../../data/transporte_ativo_2008-2019/sample_pnad2008_pos.rds")


