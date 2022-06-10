# Create survey design for PNS 2013 data 
# 1) Load packages  ------
rm(list=ls())
gc(reset=TRUE)
library(survey)
library(data.table)# devtools::install_github("lucasmation/microdadosBrasil")
library(magrittr)
# 2) Read files -----

pns2013 <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2013.rds")


pns2013$P040 %>% unique()
pns2013[!is.na(P040)
         ,v1410_todas := fifelse(P040 %like% "Sim","Sim","Não")]
pns2013$v1410_todas %>% table(.,exclude = FALSE)
pns2013$P040 %>% table(.,exclude = FALSE)
# M001    | Entrevista do adulto selecionado
#         | 1	Realizada
#         | 2	Recusa
#         | 3	Morador não encontrado
#         | 9	Ignorado
#         | Não aplicável
# UPA_PNS | UPA
# V0024   | Strata
# V00293  | Dominio de pos-estrato 2 | V00293
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
summary(pns2013Det[is.na(V0029) & M001 != 1]$V0029)
summary(pns2013Det$V0029)
summary(pns2013[!is.na(V0029) & M001 == 1]$V0029)


# Subset PNS with individuals who answered the detailed questionnaire only
# This eliminates observations with missing values in the weight variable


#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    


# Cria objeto de desenho da amostra                      
pns2013Det <- pns2013[M001 == 1]
pre_stratified <- survey::svydesign(data = pns2013Det
                                    ,id = ~UPA_PNS    # PSU
                                     ,strata = ~V0024  # Strat
                                    ,weights = ~V00291 # PesoPessoa: usar peso original
                                    ,nest = TRUE)


# apply filter
pre_stratified_total <- pre_stratified %>% 
  subset(. ,!is.na(P040)) %>% 
  subset(. ,V00291 > 0) %>% 
  update(P040_parte_trajeto = as.numeric(P040 == "Sim, parte do trajeto")) %>% 
  update(P040_todo_trajeto = as.numeric(P040 == "Sim, todo o trajeto")) %>% 
  subset(. ,C008 >= 18 )

# apply filter
pre_stratified_urbano <- pre_stratified %>% 
  subset(. ,!is.na(P040)) %>% 
  subset(. ,V00291 > 0) %>% 
  subset(. ,urban == "Urbano") %>% 
  update(P040_parte_trajeto = as.numeric(P040 == "Sim, parte do trajeto")) %>% 
  update(P040_todo_trajeto = as.numeric(P040 == "Sim, todo o trajeto")) %>% 
  subset(. ,C008 >= 18 )

pre_stratified_v1410 <- pre_stratified %>% 
  subset(. ,V00291 > 0) %>% 
  subset(. ,urban == "Urbano") %>%  
  subset(. ,P040 == "Sim, todo o trajeto") %>% 
  subset(. ,C008 >= 18 )

pre_stratified_v1410_todas <- pre_stratified %>% 
  subset(. ,V00291 > 0) %>% 
  subset(. ,dummyMetro  == "Metro")  %>% 
  subset(. ,v1410_todas == "Sim") %>% 
  subset(. ,C008 >= 18 )
## Agora é preciso pós-estratificar:
## A definição dos pós-estratos e os totais populacionais usados são dados por:

## post-estratification of sample design
# V00292  | Projecao da populacao para moradores selecionados
# V00293  | Dominio de pos-estrato 2

# defineestrata table
post.pop <-  table(V00293 = pns2013Det$V00293)

sample_pns_pos <- survey::postStratify(
  design = pre_stratified_total
  ,strata = ~V00293
  , population = post.pop
  , partial = TRUE
)

#sample_pns_pos_urban <- survey::postStratify(
#  design = pre_stratified_urbano
#  ,strata = ~V00293
#  , population = post.pop
#  , partial = TRUE
#)

system.time({
  sample_pns_pos_urban <- survey::as.svrepdesign(
    design = pre_stratified_urbano
    ,type = "bootstrap"
    ,replicates = 100
  )
})

#sample_pns_pos_v1410 <- survey::postStratify(
#  design = pre_stratified_v1410
#  , strata = ~V00293 # strata
#  , population = post.pop
#  , partial = TRUE
#)

system.time({
  sample_pns_pos_v1410 <- survey::as.svrepdesign(
    design = pre_stratified_v1410
    ,type = "bootstrap"
    ,replicates = 100
  )
})

#sample_pns_pos_v1410todas <- survey::postStratify(
#  design = pre_stratified_v1410_todas
#  , strata = ~V00293 # strata
#  , population = post.pop
#  , partial = TRUE
#)

system.time({
  sample_pns_pos_v1410todas <- survey::as.svrepdesign(
    design = pre_stratified_v1410_todas
    ,type = "bootstrap"
    ,replicates = 100
  )
})
# 4) Export values -----
# > p_actv_commute ~ situacao + region ----

future::plan("multisession",workers  = 20)
df1a <- survey::svyby( ~ P040_parte_trajeto
                       , ~ region + urban
                       , design = sample_pns_pos
                       , vartype = "ci", ci = TRUE
                       , level = 0.95, FUN = svyciprop
                       , multicore = getOption("survey.multicore"),
                       , verbose = TRUE
                       , na.rm.all = FALSE
                       , drop.empty.groups = FALSE)
df1a
df1b <- survey::svyby( ~ P040_todo_trajeto
                       , ~ region + urban
                       , design = sample_pns_pos
                       , vartype = "ci", ci = TRUE
                       , level = 0.95, FUN = svyciprop
                       , multicore = getOption("survey.multicore"),
                       , verbose = TRUE
                       , na.rm.all = FALSE
                       , drop.empty.groups = FALSE)

df1a
df1b
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sit_region.rds"
  ,x = list(
    "sit_region_1" = df1a
    ,"sit_region_2" = df1b
  )
  ,compress = "gz"
)
# > p_actv_commute ~ brazil ----

df2a <- survey::svyby(
  ~ P040_parte_trajeto 
  , ~ country
  , design = sample_pns_pos
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2a
df2b <- survey::svyby(
  ~ P040_todo_trajeto
  , ~ country
  , design = sample_pns_pos
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
  ~ P040_parte_trajeto
  , ~ country + urban
  , design = sample_pns_pos
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2c
df2d <- survey::svyby(
  ~ P040_todo_trajeto
  , ~ country + urban
  , design = sample_pns_pos
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2d
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
  ~ P040_parte_trajeto
  , ~ dummyMetro
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df3a
df3b <- survey::svyby(
  ~ P040_todo_trajeto
  , ~ dummyMetro
  , design = sample_pns_pos_urban
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

# > p_actv_commute ~ dummyMetro----

df3c <- survey::svyby(
  ~ P040_parte_trajeto
  , ~ dummyMetro + quintileDummyMetro
  , design = sample_pns_pos
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df3c
df3d <- survey::svyby(
  ~ P040_todo_trajeto
  , ~ dummyMetro + quintileDummyMetro
  , design = sample_pns_pos
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df3d
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/quintileDummyMetro.rds"
  ,x = list(
    "quintileDummyMetro_1" = df3c
    ,"quintileDummyMetro_2" = df3d
  )
  ,compress = "gz"
)

# > p_actv_commute ~ sexo  ----
df4a <- survey::svyby(
  ~ P040_parte_trajeto
  , by = ~  sexo
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4a
df4b <- survey::svyby(
   ~ P040_todo_trajeto
  , by = ~  sexo
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4a
df4b
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo.rds"
  ,x = list(
    "sexo_1" = df4a
    ,"sexo_2" = df4b
  )
  ,compress = "gz"
)

# > p_actv_commute ~ sexo + idade  ----

df4d <- survey::svyby(
  ~ P040_todo_trajeto
  , by = ~  agegroup  + sexo
  , design = sample_pns_pos
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4d

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_age.rds"
  ,x = list("sexo_age" = df4d)
  ,compress = "gz"
)

# > p_actv_commute ~ sexo + raca  ----

df4e <- survey::svyby(
  ~ P040_todo_trajeto
  , by = ~  raca_group + sexo
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4e

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_raca.rds"
  ,x = list("sexo_raca" = df4e)
  ,compress = "gz"
)

# > p_actv_commute ~ sexo + escolaridade ----

options(survey.lonely.psu = "adjust") 

df4c <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ edugroup + sexo
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4c

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_esc.rds"
  ,x = list(
    "sexo_esc" = df4c
  )
  ,compress = "gz"
)

# > p_actv_commute ~ regiao + quintileRegion  ----

df5b <- survey::svyby(
  formula = ~P040_todo_trajeto
  , by = ~ region + quintileRegion
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df5b

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/region_quint.rds"
  ,x = list("region_quint_1" = df5b)
  ,compress = "gz"
) 

# > p_actv_commute ~ BR + quintileBr  ----

df5d <- survey::svyby(
  formula = ~P040_todo_trajeto
  , by = ~ country + quintileBR
  , design = sample_pns_pos_urban
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
  ,x = list("br_quint_1" = df5d
  )
  ,compress = "gz"
) 

# > p_actv_commute ~ sexo + raca  ----

options(survey.lonely.psu = "adjust")

df6a <- survey::svyby(
  formula = ~P040_todo_trajeto
  , by = ~ sexo + raca_group
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df6a

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_race.rds"
  ,x = list(
    "sexo_race_1" = df6a
  )
  ,compress = "gz"
)

## > time_xx_to_yy  ----
options(survey.lonely.psu = "adjust") 

time_00to09 <- survey::svyby(
  formula = ~actv_commutetime_00to09
  , by = ~ sexo
  , design = sample_pns_pos_v1410todas
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
  , design = sample_pns_pos_v1410todas
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
  , design = sample_pns_pos_v1410todas
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
  , design = sample_pns_pos_v1410todas
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
  , design = sample_pns_pos_v1410todas
  , FUN = svyciprop
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
 
time_45to59

readr::write_rds(
   file = "../../data/transporte_ativo_2008-2019/export_pns13/time_dummy.rds"
   ,x = list(
     "time_00to09" = time_00to09
     ,"time_10to19" = time_10to19
     ,"time_20to29" = time_20to29
     ,"time_30to44" = time_30to44
     ,"time_45to59" = time_45to59
   )
   ,compress = "gz"
 )
# end----
