# Load libraries -----

# https://www.ibge.gov.br/estatisticas/sociais/educacao/9127-pesquisa-nacional-por-amostra-de-domicilios.html?edicao=18338&t=microdados
rm(list=ls())
gc(reset=TRUE)

library(survey)
library(magrittr)
library(data.table)
############## PNAD Create survey design  -----------

#load data set
pnad2008 <- readr::read_rds("../../data/transporte_ativo_2008-2019/pnad2008.rds")
pnad2008[,v4609 := as.numeric(v4609)]


#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html

# Handling of strata with a single PSU that are not certainty PSUs is controlled 
# by options("survey.lonely.psu").
# The default setting is "fail", which gives an error. Use "remove" to ignore that
# PSU for variance computation, "adjust" to center the stratum at the population
# mean rather than the stratum mean, and "average" to replace the variance 
# contribution of the stratum by the average variance contribution across strata.

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
anyNA(pnad2008$v4618)                # F
sum(is.na(pnad2008$v4618))           # 0
summary(pnad2008$v4617)
summary(pnad2008$v4618)
sum(is.na(pnad2008$V0029))           # 0
sum(is.na(pnad2008$V4611))           # 0
length(which(pnad2008$M001 == 1))    # 0
sum(pnad2008$v4729 == 0)

# Subset PNAD with individuals who answered the detailed questionnaire only
# # This eliminates observations with missing values in the weight variable
# 
# pnad2008Det <- data.table::copy(pnad2008)[!is.na(V0029) & M001 == 1]

options( survey.lonely.psu = "adjust")                    

pre_stratified  <- survey::svydesign(
  data = pnad2008
  #data = rbind(pnad2008,pnad2008)
  , id = ~v4618           # PSU
  , weights = ~v4729     # person weight # ? pre_wgt ?v4729
  , strata = ~v4617
  , nest = TRUE
  , multicore = TRUE
)

# apply filter and update

pre_stratified_total <- pre_stratified %>% 
  subset(. ,!is.na(v1410)) %>% 
  update(v1410sim = as.numeric(v1410 == "Sim")) %>% 
  subset(. ,v8005 >= 18 )

pre_stratified_urban <- pre_stratified %>% 
  subset(. ,!is.na(v1410)) %>% 
  update(v1410sim = as.numeric(v1410 == "Sim")) %>% 
  subset(. ,v8005 >= 18 ) %>% 
  subset(. ,urban == "Urbano") 

pre_stratified_v1410 <- pre_stratified %>% 
  subset(. ,v1410 == "Sim") %>% 
  subset(. ,v8005 >= 18 ) %>% 
  subset(. ,dummyMetro  == "Metro") 

# defineestrata table
post.pop <-  table(v4617 = pnad2008$v4617)  # v4617 = strata

pnad_design <- survey::postStratify(
  design = pre_stratified_total
  , strata = ~v4617 # strata
  , population = post.pop
  , partial = TRUE
)

#pnad_design_urban <- survey::postStratify(
#  design = pre_stratified_urban
#  , strata = ~v4617 # strata
#  , population = post.pop
#  , partial = TRUE
#)

system.time({
  pnad_design_urban <- survey::as.svrepdesign(
    design = pre_stratified_urban
    ,type = "bootstrap"
    ,replicates = 100
  )
})

pnad_design_v1410 <- survey::postStratify(
  design = pre_stratified_v1410
  , strata = ~v4617 # strata
  , population = post.pop
  , partial = TRUE
)

system.time({
  pnad_design_v1410 <- survey::as.svrepdesign(
    design = pre_stratified_v1410
    ,type = "bootstrap"
    ,replicates = 100
  )
})


# 4) Export values -----
# > p_actv ~ situacao + region ----

df1 <- survey::svyby(formula = ~ v1410sim   
                     , by =  ~ region + quintileRegion
                     , design = pnad_design
                     , vartype = "ci"
                     , ci = TRUE
                     , level = 0.95
                     , FUN = svyciprop
                     , multicore = getOption("survey.multicore")
                     , verbose = TRUE
                     , na.rm.all = TRUE
                     , drop.empty.groups = TRUE)

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/sit_region.rds"
  ,x = list(
    "sit_region" = df1
  )
  ,compress = "gz"
)
# > p_actv ~ brazil ----

future::plan("multisession",workers  = 20)
df2a <- survey::svyby(
  ~v1410sim
  , ~ country
  , design = pnad_design
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
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
# > p_actv ~ brazil + situacao----

df2c <- survey::svyby(
  ~ v1410sim
  , ~ country + urban
  , design = pnad_design
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

# > p_actv ~ dummyMetro----

df3a <- survey::svyby(
  ~ v1410sim, ~ dummyMetro
  , design = pnad_design
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

# > p_actv ~ sexo  ----

df4a <- survey::svyby(
  formula = ~ v1410sim   
  , by = ~  sexo 
  , design = pnad_design_urban
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

# > p_actv ~ sexo + idade ----

df4c <- survey::svyby(
  formula = ~ v1410sim
  , by = ~ agegroup  + sexo
  , design = pnad_design_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df4c
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/sexo_age.rds"
  ,x = list("sexo_age" = df4c)
  ,compress = "gz"
)

# > p_actv ~ sexo + escolaridade ----

df4c <- survey::svyby(
  formula = ~ v1410sim
  , by = ~ edugroup + sexo
  , design = pnad_design_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4c

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/sexo_esc.rds"
  ,x = list("sexo_esc" = df4c)
  ,compress = "gz"
)


# > p_actv ~ sexo + raca  ----

options(survey.lonely.psu = "adjust") 

df6a <- survey::svyby(
  formula = ~v1410sim
  , by = ~ sexo + raca_group
  , design = pnad_design_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df6a
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/sexo_race.rds"
  ,x = list("sexo_race" = df6a)
  ,compress = "gz"
) 

# > p_actv ~ sexo + raca +  escolaridade ----

df4f <- survey::svyby(
  formula = ~ v1410sim
  , by = ~ edugroup + sexo + raca_group 
  , design = pnad_design_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df4f

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/sexo_raca_esc.rds"
  ,x = list("sexo_raca_esc" = df4f)
  ,compress = "gz"
)

# > p_actv ~ regiao + quintileRegion  ----

df5a <- survey::svyby(
  formula = ~v1410sim
  , by = ~ region + quintileRegion
  , design = pnad_design_urban
  , vartype = "ci"
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df5a


df5a
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/region_quint.rds"
  ,x = list(
    "region_quint" = df5a
  )
  ,compress = "gz"
) 
# > p_actv ~ BR + quintileBr  ----


df5 <- survey::svyby(
  formula = ~v1410sim
  , by = ~ country + quintileBR
  , design = pnad_design_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df5

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/br_quint.rds"
  ,x = list(
    "br_quint" = df5
  )
  ,compress = "gz"
) 


## > time_xx_to_yy  ----
options(survey.lonely.psu = "adjust") 

time_00to09 <- survey::svyby(
  formula = ~actv_commutetime_00to09
  , by = ~ sexo
  , design = pnad_design_v1410
  , FUN = svyciprop
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_00to09

time_10to19 <- survey::svyby(
  formula = ~actv_commutetime_10to19
  , by = ~ sexo
  , design = pnad_design_v1410
  , FUN = svyciprop
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_10to19

time_20to29 <- survey::svyby(
  formula = ~actv_commutetime_20to29
  , by = ~ sexo
  , design = pnad_design_v1410
  , FUN = svyciprop
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_20to29

time_30to44 <- survey::svyby(
  formula = ~actv_commutetime_30to44
  , by = ~ sexo
  , design =pnad_design_v1410
  , FUN = svyciprop
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_30to44

time_45to59 <- survey::svyby(
  formula = ~actv_commutetime_45to59
  , by = ~ sexo
  , design = pnad_design
  , FUN = svyciprop
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_45to59

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad08/time_dummy.rds"
  ,x = list(
    "time_00to09"  = time_00to09[,1:2]
    ,"time_10to19" = time_10to19[,1:2]
    ,"time_20to29" = time_20to29[,1:2]
    ,"time_30to44" = time_30to44[,1:2]
    ,"time_45to59" = time_45to59[,1:2]
  )
  ,compress = "gz"
)
# end----
