# Create survey design for PNS 2019 data 
# 1) Load packages  ------
rm(list=ls())
gc(reset=TRUE)
library(survey)
library(data.table)
library(magrittr)
# 2) Read files -----

pns2019 <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2019.rds")

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

anyNA(pns2019$V00291)                # TRUE
sum(is.na(pns2019$V00291))           # 202880
sum(is.na(pns2019$V0029))            # 202880
length(which(pns2019$M001 == 1))     # 90846
summary(pns2019[M001 != 1,]$V00291)
summary(pns2019[M001 == 1,]$V0029)


#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    


# Cria objeto de desenho da amostra                      

pns2019det <- pns2019[M001 == 1,]
pre_stratified <- survey::svydesign(data = pns2019det
                                    , strata = ~V0024
                                    , id = ~UPA_PNS    # PSU
                                    , weights = ~V00291 # PesoPessoa: usar peso original
                                    , nest = TRUE)


# filtro
pre_stratified_total <- pre_stratified %>% 
  subset(. ,!is.na(P040)) %>% 
  update(P040_parte_trajeto = as.numeric(P040 == "Sim, parte do trajeto")) %>% 
  update(P040_todo_trajeto = as.numeric(P040 == "Sim, todo o trajeto")) %>% 
  subset(. ,C008 >= 18 )

# filtro
pre_stratified_urbano <- pre_stratified %>% 
  subset(. ,!is.na(P040)) %>% 
  subset(. ,C008 >= 18 ) %>% 
  subset(. ,urban == "Urbano") %>% 
  update(P040_parte_trajeto = as.numeric(P040 == "Sim, parte do trajeto")) %>% 
  update(P040_todo_trajeto = as.numeric(P040 == "Sim, todo o trajeto")) 

# filtro
pre_stratified_v1410 <- pre_stratified %>% 
  subset(. ,!is.na(P040)) %>% 
  subset(. ,C008 >= 18 ) %>% 
  subset(. ,urban == "Urbano") %>% 
  subset(. ,P040 == "Sim, todo o trajeto") 

# filtro
pre_stratified_v1410todas <- pre_stratified %>% 
  subset(. ,!is.na(P040)) %>% 
  subset(. ,C008 >= 18 ) %>% 
  subset(. ,dummyMetro  == "Metro")  %>% 
  subset(.,v1410 == "Sim") 

## Agora é preciso pós-estratificar:
## A definição dos pós-estratos e os totais populacionais usados são dados por:

tmp_rep <- survey::as.svrepdesign(
  design = pre_stratified_urbano
  ,type = "bootstrap"
  ,replicates = 2000)


## post-estratification of sample design
# V00292  | Projecao da populacao para moradores selecionados
# V00293  | Dominio de pos-estrato 2

# defineestrata table
post.pop <-  table(V0024 = pns2019det$V0024)

sample_pns_pos <- survey::postStratify(
  design = pre_stratified_total
  ,strata = ~V0024
  , population = post.pop
  , partial = TRUE
)


sample_pns_pos_urban <- survey::postStratify(
  design = pre_stratified_urbano
  ,strata = ~V0024
  , population = post.pop
  , partial = TRUE
)

system.time({
  sample_pns_pos_v1410 <- survey::as.svrepdesign(
    design = pre_stratified_v1410
    ,type = "bootstrap"
    ,replicates = 100
  )
})

 #- 
sample_pns_pos_v1410 <- survey::postStratify(
  design = pre_stratified_v1410
  , strata = ~V0024 # strata
  , population = post.pop
  , partial = TRUE
)

system.time({
  sample_pns_pos_v1410 <- survey::as.svrepdesign(
    design = pre_stratified_v1410
    ,type = "bootstrap"
    ,replicates = 100
  )
})

# -
sample_pns_pos_v1410todas <- survey::postStratify(
  design = pre_stratified_v1410todas
  , strata = ~V0024 # strata
  , population = post.pop
  , partial = TRUE
)

system.time({
  sample_pns_pos_v1410 <- survey::as.svrepdesign(
    design = pre_stratified_v1410
    ,type = "bootstrap"
    ,replicates = 100
  )
})

# 4) Export values -----
# > p_actv ~ situacao + region ----

future::plan("multisession",workers  = 20)
df1 <- survey::svyby(~ P040_todo_trajeto
                     , ~ region + urban
                     , design = sample_pns_pos
                     , vartype = "ci", ci = TRUE
                     , level = 0.95, FUN = svyciprop
                     , multicore = getOption("survey.multicore"),
                     , verbose = TRUE
                     , na.rm.all = FALSE
                     , drop.empty.groups = FALSE)
df1

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sit_region.rds"
  ,x = list("sit_region_1" = df1)
  ,compress = "gz"
)

# > p_actv ~ brazil ----

df2a <- survey::svyby(
  ~ P040_parte_trajeto, ~ country
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
  ~ P040_todo_trajeto, ~ country
  , design = sample_pns_pos
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2b

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/br.rds"
  ,x = list(
    "brasil_1" = df2a
    ,"brasil_2" = df2b
  )
  ,compress = "gz"
)
# > p_actv ~ brazil + situacao----

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
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df2d
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sit_brasil.rds"
  ,x = list(
    "sit_brasil_1" = df2c
    ,"sit_brasil_2" = df2d
  )
  ,compress = "gz"
)

# > p_actv ~ dummyMetro----

df3a <- survey::svyby(
  ~ P040_parte_trajeto, ~ dummyMetro
  , design = sample_pns_pos
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
  ~ P040_todo_trajeto, ~ dummyMetro
  , design = sample_pns_pos
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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/dummyMetro.rds"
  ,x = list(
    "dummyMetro_1" = df3a
    ,"dummyMetro_2" = df3b
  )
  ,compress = "gz"
)

# > p_actv ~ dummyMetro + Quintile----

df3c <- survey::svyby(
  ~ P040_parte_trajeto, ~ dummyMetro + quintileDummyMetro
  , design = sample_pns_pos
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)
df3c
df3d <- survey::svyby(
  ~ P040_todo_trajeto, ~ dummyMetro + quintileDummyMetro
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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/quintileDummyMetro.rds"
  ,x = list(
    "quintileDummyMetro_1" = df3c
    ,"quintileDummyMetro_2" = df3d
  )
  ,compress = "gz"
)
# > p_actv ~ sexo  ----
df4a <- survey::svyby(
  formula = ~ P040_parte_trajeto
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
  formula = ~ P040_todo_trajeto
  , by = ~  sexo
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4b
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo.rds"
  ,x = list(
    "sexo_1" = df4a
    ,"sexo_2" = df4b
  )
  ,compress = "gz"
)

# > p_actv ~ sexo + idade  ----
options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ agegroup + sexo
  , design = sample_pns_pos_urban
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4d

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo_age.rds"
  ,x = list("sexo_age" = df4d )
  ,compress = "gz"
)


# > p_actv ~ sexo + escolaridade ----
options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
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
df4d

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo_esc.rds"
  ,x = list("sexo_esc" = df4d )
  ,compress = "gz"
)

# > p_actv ~ sexo + quintil + Metro ----
options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ metro + quintileMetro + sexo
  , design = tmp_rep
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

setDT(df4d)
df4d[sexo == "Feminino"]

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo_metro_quint.rds"
  ,x = df4d
  ,compress = "gz"
)
# > p_actv ~ regiao + quintileRegion  ----

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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/region_quint.rds"
  ,x = list(
    "region_quint_1" = df5b
  )
  ,compress = "gz"
) 

# > p_actv ~ BR + quintileBr  ----


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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/br_quint.rds"
  ,x = list(
    "br_quint_1" = df5d
  )
  ,compress = "gz"
)
# > p_actv ~ sexo + raca  ----

options(survey.lonely.psu = "adjust") 

df6b <- survey::svyby(
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
df6b


readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo_race.rds"
  ,x = list("sexo_race_1" = df6b)
  ,compress = "gz"
) 

## > time_xx_to_yy  ----

time_00to09 <- survey::svyby(
  formula = ~actv_commutetime_00to09
  , by = ~ sexo
  , design = pre_stratified_v1410todas
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
  , design = pre_stratified_v1410todas
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
  , design = pre_stratified_v1410todas
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
  , design = pre_stratified_v1410todas
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
  , design = pre_stratified_v1410todas
  , FUN = svyciprop
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_45to59

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/time_dummy.rds"
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
