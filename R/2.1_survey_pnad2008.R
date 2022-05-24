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

# Handling of strata with a single PSU that are not certainty PSUs is controlled 
# by options("survey.lonely.psu").
# The default setting is "fail", which gives an error. Use "remove" to ignore that
# PSU for variance computation, "adjust" to center the stratum at the population
# mean rather than the stratum mean, and "average" to replace the variance 
# contribution of the stratum by the average variance contribution across strata.

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

anyNA(pnad2008$V4729)                # F
sum(is.na(pnad2008$V4729))           # 0
summary(pnad2008$v4729)
summary(pnad2008$pre_wgt)
sum(is.na(pnad2008$V0029))           # 0
sum(is.na(pnad2008$V4611))           # 0
length(which(pnad2008$M001 == 1))    # 0
sum(pnad2008$v4729 == 0)

# Subset PNAD with individuals who answered the detailed questionnaire only
# # This eliminates observations with missing values in the weight variable
# 
# pnad2008Det <- data.table::copy(pnad2008)[!is.na(V0029) & M001 == 1]


# Cria objeto de desenho da amostra                      
sample.pnad08 <- survey::svydesign(data = pnad2008
                           , id = ~v4618              # PSU
                           , strata = ~v4617          # Strat
                           , weights = ~v4729         # person weight # ? pre_wgt ?v4729
                           #, fpc = ~as.numeric(v4609) # projecao da populacao
                           , nest = TRUE
                           , multicore = TRUE)

# postStratify pnad2008 (Djalma suggestion)
# v4609 => projecao da populacao
# V4617		STRAT - Identificação de estrato de município auto-representativo e não auto-representativo

post.pop <- data.frame(v4609 = as.character(pnad2008$v4609)
                       , Freq = as.numeric(pnad2008$v4609))
post.pop <- unique(post.pop)
sample.pos <- survey::postStratify(design = sample.pnad08
                                   , strata = ~v4609 # v4609
                                   , population = post.pop)

# 
# # Subset Sample of survey design for aged above 14 years old
sample.pnad08.14y <- subset(sample.pos, v8005>=14)

# 3) Save survey ------
readr::write_rds(x = sample.pos
        ,file = "../../data/transporte_ativo_2008-2019/sample_pnad2008_pos.rds",compress= "gz")


# df3 <- survey::svyby(formula = ~factor(v1410 == "Sim"), by = ~ region + sexo 
#                      , design = sample.pnad08.14y, 
#                      , vartype = "ci", ci = TRUE
#                      , level = 0.95, FUN = svyciprop
#                      #, method = "likelihood"
#                      #, multicore = TRUE,
#                      , verbose = TRUE
#                      , na.rm.all = FALSE
#                      , drop.empty.groups = FALSE)
# df3
# df4 <- survey::svyby(formula = ~factor(v1410 == "Sim"), by = ~ region + agegroup1 
#                      , design = subset(sample.pnad08.14y,region == "Nordeste"), 
#                      , vartype = "ci", ci = TRUE, level = 0.95, FUN = svyciprop
#                      #, FUN = svymean
#                      #, method = "likelihood"
#                      #, multicore = TRUE,
#                      , verbose = TRUE)
# df4
# sample.pos$variables[edugroup == "Superior completo" & sexo == "Masculino"
#                      & v1410 == "Sim",]
