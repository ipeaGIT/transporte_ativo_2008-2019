# Create survey design for PNS 2013 data 
# 1) Load packages  ------
rm(list=ls())
gc(reset=TRUE)
library(PNADcIBGE) # devtools::install_github("Gabriel-Assuncao/PNADcIBGE")
library(survey)
library(srvyr)
library(data.table)
library(microdadosBrasil) # devtools::install_github("lucasmation/microdadosBrasil")

# 2) Read files -----

pns2013 <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2013.rds")


# M001    | Entrevista do adulto selecionado
#         | 1	Realizada
#         | 2	Recusa
#         | 3	Morador não encontrado
#         | 9	Ignorado
#         | Não aplicável
# UPA_PNS | UPA
# V0024   | Strata
# V00293  | Dominio de pos-estrato 2
# V0029	  | Peso do morador selecionado sem calibracao
# V00291  | Peso do morador selecionado com calibracao
# V00292  | Projecao da populacao para moradores selecionados
# V00283  | Domínio de projeção para domicílio e moradores

# There should be no Missings (NA) in Design Variables
# Count  missing values (NAs)

anyNA(pns2013$V00291)                # TRUE
sum(is.na(pns2013$V00291))           # 162183
sum(is.na(pns2013$V0029))            # 162183
length(which(pns2013$M001 == 1))     # 60202


# Subset PNS with individuals who answered the detailed questionnaire only
# This eliminates observations with missing values in the weight variable

pns2013Det <- data.table::copy(pns2013)[!is.na(V00291) & M001 == 1]



#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    


# Cria objeto de desenho da amostra                      

sample_pns <- survey::svydesign(data = pns2013Det,
                                id = ~UPA_PNS,    # PSU
                                strata = ~V0024,  # Strat
                                weights = ~V0029, # PesoPessoa: usar peso original
                                nest = TRUE)

## Agora é preciso pós-estratificar:
## A definição dos pós-estratos e os totais populacionais usados são dados por:

## post-estratification of sample design
# V00292  | Projecao da populacao para moradores selecionados
# V00293  | Dominio de pos-estrato 2

post_pop <- unique( pns2013Det[,c("V00293","V00292")] )
names(post_pop) <- c("V00293","Freq")

sample_pns_pos <- survey::postStratify(sample_pns, ~V00293, post_pop)


# 3) filter------ 
pns13_18y <- subset(sample_pns_pos, C008 >= 18)

# 4) Export values -----
# > p_actv_commute ~ situacao + region ----

# future::plan("multisession",workers  = 20)
# df1 <- survey::svyby(~ factor(v1410 == "Sim"), ~ region + urban
#                      , design = pns13_18y
#                      , vartype = "ci", ci = TRUE
#                      , level = 0.95, FUN = svyciprop
#                      #, method = "likelihood"
#                      , multicore = getOption("survey.multicore"),
#                      , verbose = TRUE
#                      , na.rm.all = FALSE
#                      , drop.empty.groups = FALSE)
# df1
#df1a <- survey::svyby( ~ factor(P040  == "Sim, parte do trajeto")
#                       , ~ region + urban
#                       , design = pns13_18y
#                       , vartype = "ci", ci = TRUE
#                       , level = 0.95, FUN = svyciprop
#                       #, method = "likelihood"
#                       , multicore = getOption("survey.multicore"),
#                       , verbose = TRUE
#                       , na.rm.all = FALSE
#                       , drop.empty.groups = FALSE)
#df1a
#df1b <- survey::svyby( ~ factor(P040  == "Sim, todo o trajeto")
#                       , ~ region + urban
#                       , design = pns13_18y
#                       , vartype = "ci", ci = TRUE
#                       , level = 0.95, FUN = svyciprop
#                       #, method = "likelihood"
#                       , multicore = getOption("survey.multicore"),
#                       , verbose = TRUE
#                       , na.rm.all = FALSE
#                       , drop.empty.groups = FALSE)


# readr::write_rds(
#   file = "../../data/transporte_ativo_2008-2019/export_pns13/sit_region.rds"
#   ,x = list(
#    "sit_region_1" = df1a
#   ,"sit_region_2" = df1b
#   )
#   ,compress = "gz"
# )
# > p_actv_commute ~ brazil ----

df2a <- survey::svyby(
  ~ factor(P040  == "Sim, parte do trajeto"), ~ country
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2a
df2b <- survey::svyby(
  ~ factor(P040  == "Sim, todo o trajeto"), ~ country
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2b

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/br.rds"
  ,x = list(
    "brasil_1" = df2a
    ,"brasil_2" = df2b
  )
  ,compress = "gz"
)
# > p_actv_commute ~ brazil + situacao----

df2c <- survey::svyby(
  ~ factor(P040  == "Sim, parte do trajeto")
  , ~ country + urban
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2c
df2d <- survey::svyby(
  ~ factor(P040  == "Sim, todo o trajeto")
  , ~ country + urban
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sit_brasil.rds"
  ,x = list(
    "sit_brasil_1" = df2c
    ,"sit_brasil_2" = df2d
  )
  ,compress = "gz"
)

# > p_actv_commute ~ dummyMetro----

df3a <- survey::svyby(
  ~ factor(P040  == "Sim, parte do trajeto"), ~ dummyMetro
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df3a
df3b <- survey::svyby(
  ~ factor(P040  == "Sim, todo o trajeto"), ~ dummyMetro
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df3b
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/dummyMetro.rds"
  ,x = list(
    "dummyMetro_1" = df3a
    ,"dummyMetro_2" = df3b
  )
  ,compress = "gz"
)

# > p_actv_commute ~ sexo  ----
df4a <- survey::svyby(
  formula = ~ factor(P040  == "Sim, parte do trajeto")
  , by = ~  sexo
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4a
df4b <- survey::svyby(
  formula = ~ factor(P040  == "Sim, todo o trajeto")
  , by = ~  sexo
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo.rds"
  ,x = list(
    "sexo_1" = df4a
    ,"sexo_2" = df4b
  )
  ,compress = "gz"
)

# > p_actv_commute ~ sexo + escolaridade ----
# options(survey.lonely.psu = "adjust") 
# 
# df4c <- survey::svyby(
#   formula = ~ factor(P040  == "Sim, parte do trajeto")
#   , by = ~ edugroup + sexo
#   , design = pns13_18y
#   , vartype = "ci", ci = TRUE
#   , level = 0.95, FUN = svyciprop
#   , multicore = getOption("survey.multicore")
#   , verbose = TRUE
#   , na.rm.all = TRUE
#   , drop.empty.groups = TRUE
# )
# df4c
# df4d <- survey::svyby(
#   formula = ~ factor(P040  == "Sim, todo o trajeto")
#   , by = ~ edugroup + sexo
#   , design = pns13_18y
#   , vartype = "ci", ci = TRUE
#   , level = 0.95, FUN = svyciprop
#   , multicore = getOption("survey.multicore")
#   , verbose = TRUE
#   , na.rm.all = TRUE
#   , drop.empty.groups = TRUE
# )
# df4d
# 
# pns13_18y$variables[v1410 == "Sim",.N,by=.(edugroup,sexo)]

# readr::write_rds(
#   file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_esc.rds"
#   ,x = list(
#     "sexo_esc_1" = df4c
#     ,"sexo_esc_2" = df4d
#   )
#   ,compress = "gz"
# )
# 
# > p_actv_commute ~ regiao + quintileRegion  ----


df5a <- survey::svyby(
 formula = ~factor(v1410 == "Sim, parte do trajeto")
 , by = ~ region + quintileRegion
 , design = pns13_18y
 , vartype = "ci", ci = TRUE
 , level = 0.95
 , FUN = svyciprop
 , multicore = getOption("survey.multicore")
 , verbose = TRUE
 , na.rm.all = TRUE
 , drop.empty.groups = TRUE
)
df5a
df5b <- survey::svyby(
 formula = ~factor(v1410 == "Sim, todo o trajeto")
 , by = ~ region + quintileRegion
 , design = pns13_18y
 , vartype = "ci", ci = TRUE
 , level = 0.95
 , FUN = svyciprop
 , multicore = getOption("survey.multicore")
 , verbose = TRUE
 , na.rm.all = TRUE
 , drop.empty.groups = TRUE
)
df5b
df5b

pns13_18y$variables[v1410 == "Sim",.N,by=.(region,quintileRegion)]

readr::write_rds(
file = "../../data/transporte_ativo_2008-2019/export_pns13/region_quint.rds"
,x = list(
  "region_quint_1" = df5a
  ,"region_quint_2" = df5b
)
,compress = "gz"
) 

# > p_actv_commute ~ BR + quintileBr  ----


df5c <- survey::svyby(
  formula = ~factor(v1410 == "Sim, parte do trajeto")
  , by = ~ quintileBR
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df5c
df5d <- survey::svyby(
  formula = ~factor(v1410 == "Sim, todo o trajeto")
  , by = ~ quintileBR
  , design = pns13_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df5d

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/br_quint.rds"
  ,x = list(
    "br_quint_1" = df5c
    ,"br_quint_2" = df5d
  )
  ,compress = "gz"
) 
# > p_actv_commute ~ sexo + raca  ----

#options(survey.lonely.psu = "adjust") 
#
#df6a <- survey::svyby(
#  formula = ~factor(v1410 == "Sim, parte do trajeto")
#  , by = ~ sexo + raca_group
#  , design = pns13_18y
#  #, design = subset(pns13_18y,region == "Sul") 
#  , vartype = "ci", ci = TRUE
#  , level = 0.95
#  , FUN = svyciprop
#  #, multicore = TRUE,
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#df6b <- survey::svyby(
#  formula = ~factor(v1410 == "Sim, todo o trajeto")
#  , by = ~ sexo + raca_group
#  , design = pns13_18y
#  #, design = subset(pns13_18y,region == "Sul") 
#  , vartype = "ci", ci = TRUE
#  , level = 0.95
#  , FUN = svyciprop
#  #, multicore = TRUE,
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#pns13_18y$variables[v1410 == "Sim",.N,by=.(region,quintileRegion)]


# readr::write_rds(
#  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_race.rds"
#  ,x = list(
#    "sexo_race_1" = df6a
#    "sexo_race_2" = df6b
#  )
#  ,compress = "gz"
# ) 

## > time_xx_to_yy  ----
#break()
#options(survey.lonely.psu = "adjust") 
#
#pns13_18y$variables[
#  region == "Sul"
#  ,.N
#  , by =.(region,quintileRegion,actv_commutetime_00to09)
#]
#
#time_00to09 <- survey::svyby(
#  formula = ~factor(actv_commutetime_00to09==1)
#  , by = ~ region + quintileRegion
#  , design = pns13_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#time_10to19 <- survey::svyby(
#  formula = ~factor(actv_commutetime_10to19==1)
#  , by = ~ region + quintileRegion
#  , design = pns13_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#time_20to29 <- survey::svyby(
#  formula = ~factor(actv_commutetime_20to29==1)
#  , by = ~ region + quintileRegion
#  , design = pns13_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#time_30to44 <- survey::svyby(
#  formula = ~factor(actv_commutetime_30to44==1)
#  , by = ~ region + quintileRegion
#  , design = pns13_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#time_45to59 <- survey::svyby(
#  formula = ~factor(actv_commutetime_45to59==1)
#  , by = ~ region + quintileRegion
#  , design = pns13_18y
#  , FUN = svyciprop
#  , multicore=getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
# readr::write_rds(
#   file = "../../data/transporte_ativo_2008-2019/export_pns13/time_xxtoyy.rds"
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
