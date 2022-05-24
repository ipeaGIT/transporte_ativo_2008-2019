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

# 3) filter------ 
pnad_18y <- subset(sample.pos, v8005 >= 18)
# 4) Export values -----
# > p_actv_commute ~ situacao + region ----
#df1a <- survey::svyby( ~ factor(v1410 == "Sim")
#                       , ~ region + urban
#                       , design = pnad_18y
#                       , vartype = "ci", ci = TRUE
#                       , level = 0.95, FUN = svyciprop
#                       #, method = "likelihood"
#                       , multicore = getOption("survey.multicore"),
#                       , verbose = TRUE
#                       , na.rm.all = FALSE
#                       , drop.empty.groups = FALSE)

# readr::write_rds(
#   file = "../../data/transporte_ativo_2008-2019/export_pnad08/sit_region.rds"
#   ,x = list(
#    "sit_region" = df1a
#   )
#   ,compress = "gz"
# )
# > p_actv_commute ~ brazil ----

future::plan("multisession",workers  = 20)
df2a <- survey::svyby(
  ~ factor(v1410 == "Sim"), ~ country
  , design = pnad_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2a

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/br.rds"
  ,x = list(
    "brasil" = df2a
  )
  ,compress = "gz"
)
# > p_actv_commute ~ brazil + situacao----

df2c <- survey::svyby(
  ~ factor(v1410 == "Sim")
  , ~ country + urban
  , design = pnad_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2c

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/sit_brasil.rds"
  ,x = list(
    "sit_brasil" = df2c
  )
  ,compress = "gz"
)

# > p_actv_commute ~ dummyMetro----

df3a <- survey::svyby(
  ~ factor(v1410 == "Sim"), ~ dummyMetro
  , design = pnad_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df3a
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/dummyMetro.rds"
  ,x = list(
    "dummyMetro" = df3a
  )
  ,compress = "gz"
)

# > p_actv_commute ~ sexo  ----
df4a <- survey::svyby(
  formula = ~ factor(v1410 == "Sim")
  , by = ~  sexo
  , design = pnad_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4a
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/sexo.rds"
  ,x = list(
    "sexo" = df4a
  )
  ,compress = "gz"
)

# > p_actv_commute ~ sexo + escolaridade ----
# df4c <- survey::svyby(
#   formula = ~ factor(v1410 == "Sim")
#   , by = ~ edugroup + sexo
#   , design = pnad_18y
#   , vartype = "ci", ci = TRUE
#   , level = 0.95, FUN = svyciprop
#   , multicore = getOption("survey.multicore")
#   , verbose = TRUE
#   , na.rm.all = TRUE
#   , drop.empty.groups = TRUE
# )
# df4c
# 
# pnad_18y$variables[v1410 == "Sim",.N,by=.(edugroup,sexo)]

# readr::write_rds(
#   file = "../../data/transporte_ativo_2008-2019/export_pnad08/sexo_esc.rds"
#   ,x = list(
#     "sexo_esc" = df4c
#   )
#   ,compress = "gz"
# )
# 
# > p_actv_commute ~ regiao + quintileRegion  ----
# df5a <- survey::svyby(
#   formula = ~factor(v1410 == "Sim, parte do trajeto")
#   , by = ~ region + quintileRegion
#   , design = pnad_18y
#   , vartype = "ci", ci = TRUE
#   , level = 0.95
#   , FUN = svyciprop
#   , multicore = getOption("survey.multicore")
#   , verbose = TRUE
#   , na.rm.all = TRUE
#   , drop.empty.groups = TRUE
# )
# readr::write_rds(
#  file = "../../data/transporte_ativo_2008-2019/export_pnad08/region_quint.rds"
#  ,x = list(
#    "region_quint" = df5a
#  )
#  ,compress = "gz"
# ) 
# > p_actv_commute ~ sexo + raca  ----

#options(survey.lonely.psu = "adjust") 
#
#df6a <- survey::svyby(
#  formula = ~factor(v1410 == "Sim, parte do trajeto")
#  , by = ~ sexo + raca_group
#  , design = pnad_18y
#  #, design = subset(pnad_18y,region == "Sul") 
#  , vartype = "ci", ci = TRUE
#  , level = 0.95
#  , FUN = svyciprop
#  #, multicore = TRUE,
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)

# readr::write_rds(
#  file = "../../data/transporte_ativo_2008-2019/export_pnad08/sexo_race.rds"
#  ,x = list(
#    "sexo_race" = df6a
#  )
#  ,compress = "gz"
# ) 

## > time_xx_to_yy  ----
#break()
#options(survey.lonely.psu = "adjust") 
#
#pnad_18y$variables[
#  region == "Sul"
#  ,.N
#  , by =.(region,quintileRegion,actv_commutetime_00to09)
#]
#
#time_00to09 <- survey::svyby(
#  formula = ~factor(actv_commutetime_00to09==1)
#  , by = ~ region + quintileRegion
#  , design = pnad_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#time_10to19 <- survey::svyby(
#  formula = ~factor(actv_commutetime_10to19==1)
#  , by = ~ region + quintileRegion
#  , design = pnad_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#time_20to29 <- survey::svyby(
#  formula = ~factor(actv_commutetime_20to29==1)
#  , by = ~ region + quintileRegion
#  , design = pnad_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#time_30to44 <- survey::svyby(
#  formula = ~factor(actv_commutetime_30to44==1)
#  , by = ~ region + quintileRegion
#  , design = pnad_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#time_45to59 <- survey::svyby(
#  formula = ~factor(actv_commutetime_45to59==1)
#  , by = ~ region + quintileRegion
#  , design = pnad_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
# readr::write_rds(
#   file = "../../data/transporte_ativo_2008-2019/export_pnad08/time_xxtoyy.rds"
#   ,x = list(
#     "time_00to09" = time_00to09
#     ,"time_10to19" = time_10to19
#     ,"time_20to29" = time_20to29
#     ,"time_30to44" = time_30to44
#     ,"time_45to59" = time_45to59
#   )
#   ,compress = "gz"
# )
# end----
