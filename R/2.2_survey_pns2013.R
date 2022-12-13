# Create survey design for PNS 2013 data 
# 1) Load packages  ------
rm(list=ls())
gc(reset=TRUE)
library(survey)
library(data.table)# devtools::install_github("lucasmation/microdadosBrasil")
library(magrittr)
# 2) Read files -----

pns2013_dt <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2013_dt.rds")

# M001    | Entrevista do adulto selecionado
#         | 1	Realizada
#         | 2	Recusa
#         | 3	Morador não encontrado
#         | 9	Ignorado
#         | NA - Não aplicável
# UPA_PNS | UPA
# V0024   | Strata
# V00293  | Dominio de pos-estrato 2 | V00293
# V0029	  | Peso do morador selecionado sem calibracao
# V00291  | Peso do morador selecionado com calibracao
# V00292  | Projecao da populacao para moradores selecionados
# V00283  | Domínio de projeção para domicílio e moradores


# Subset PNS with individuals who answered the detailed questionnaire only
# This eliminates observations with missing values in the weight variable
pns2013_dt[,V00293 := as.numeric(V00293)]
pns2013_dt[,V0015 := as.numeric(V0015)]
pns2013_dt[,V00292 := as.numeric(V00292)]

pns2013_dt <- pns2013_dt[M001 == 1]
pns2013_dt <- pns2013_dt[!is.na(peso_morador_selec)]
pns2013_dt$V0029 %>% summary()
pns2013_dt$V00291 %>% summary()
pns2013_dt$V0028 %>% summary()

#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
# 3) Create design ----
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    

pre_stratified <- survey::svydesign(data = pns2013_dt
                                    , id = ~UPA_PNS     # PSU
                                    , strata = ~V0024   # Strat
                                    , weights = ~peso_morador_selec # PesoPessoa: usar peso original
                                    , nest = TRUE)


# apply filter
pres_total <- pre_stratified  %>% 
  subset(. ,!is.na(P040)) %>% 
  update(P040_parte_trajeto = as.numeric(P040 == "Sim, parte do trajeto")) %>% 
  update(P040_todo_trajeto = as.numeric(P040 == "Sim, todo o trajeto")) %>% 
  subset(. ,C008 >= 18 )

pres_urbano <- pres_total %>% 
  subset(. ,urban == "Urbano")

pres_todo_traj <- pres_urbano %>% 
  subset(. ,P040 == "Sim, todo o trajeto") 

pres_todo_parte_traj <- pres_urbano %>% 
  subset(. ,(P040 == "Sim, todo o trajeto" |
               P040 == "Sim, parte do trajeto")) 

pres_todo_parte_traj_metro <- pres_urbano %>% 
  subset(. ,dummyMetro  == "Metro")  %>% 
  subset(. ,(P040 == "Sim, todo o trajeto" |
               P040 == "Sim, parte do trajeto")) 

post_stratify = TRUE
# 4) Post-stratify -----

if(post_stratify){
  
  system.time({
    pos_total <- survey::as.svrepdesign(
      design = pres_total
      ,type = "bootstrap"
      ,replicates = 100
    )
  })
  
  system.time({
    pos_urbano <- survey::as.svrepdesign(
      design = pres_urbano
      ,type = "bootstrap"
      ,replicates = 100
    )
  })
  
  system.time({
    pos_todo_traj <- survey::as.svrepdesign(
      design = pres_todo_traj
      ,type = "bootstrap"
      ,replicates = 100
    )
  })
  
  system.time({
    pos_todo_parte_traj <- survey::as.svrepdesign(
      design = pres_todo_parte_traj
      ,type = "bootstrap"
      ,replicates = 100
    )
  })
  
  system.time({
    pos_todo_parte_traj_metro <- survey::as.svrepdesign(
      design = pres_todo_parte_traj_metro
      ,type = "bootstrap"
      ,replicates = 100
    )
  })
}else{
  pos_total <- pres_total
  pos_urbano <- pres_urbano
  pos_todo_traj <- pres_todo_traj
  pos_todo_parte_traj <- pres_todo_parte_traj
  pos_todo_parte_traj_metro <- pres_todo_parte_traj_metro
}

# 4) Export values -----
# > p_actv ~ situacao + region ----

future::plan("multisession",workers  = 20)
df1a <- survey::svyby( ~ P040_parte_trajeto
                       , ~ region + urban
                       , design = pos_total
                       , vartype = "ci", ci = TRUE
                       , level = 0.95, FUN = svyciprop
                       , multicore = getOption("survey.multicore"),
                       , verbose = TRUE
                       , na.rm.all = FALSE
                       , drop.empty.groups = FALSE)

df1a

df1b <- survey::svyby( ~ P040_todo_trajeto
                       , ~ region + urban
                       , design = pos_total
                       , vartype = "ci", ci = TRUE
                       , level = 0.95, FUN = svyciprop
                       , multicore = getOption("survey.multicore"),
                       , verbose = TRUE
                       , na.rm.all = FALSE
                       , drop.empty.groups = FALSE)

df1b
readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sit_region.rds"
  ,x = list(
    "sit_region_1" = df1a
    , "sit_region_2" = df1b
  )
  ,compress = "gz"
)
# > p_actv ~ brazil ----

df2a <- survey::svyby(
  ~ P040_parte_trajeto 
  , ~ country
  , design = pos_total
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
  , design = pos_total
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
# > p_actv ~ brazil + situacao----

df2c <- survey::svyby(
  ~ P040_parte_trajeto
  , ~ country + urban
  , design = pos_total
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
  , design = pos_total
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


# > p_actv ~ dummyMetro----

df3a <- survey::svyby(
  ~ P040_parte_trajeto
  , ~ dummyMetro
  , design = pos_urbano
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
  , design = pos_urbano
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

# > p_actv ~ QuantileDummyMetro----

df3c <- survey::svyby(
  ~ P040_parte_trajeto
  , ~ dummyMetro + quintileDummyMetro
  , design = pos_urbano
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
  , design = pos_urbano
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
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

# > p_actv ~ sexo  ----

df4a <- survey::svyby(
  ~ P040_parte_trajeto
  , by = ~  sexo
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df4a

df4b <- survey::svyby(
  ~ P040_todo_trajeto
  , by = ~  sexo
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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

# > p_actv ~ sexo + age  ----

df4d <- survey::svyby(
  ~ P040_todo_trajeto
  , by = ~  agegroup  + sexo
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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

# > p_actv ~ sexo + ageLarge  ----

options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ AGE + sexo
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df4d

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_ageLarge.rds"
  ,x = list("sexo_age" = df4d )
  ,compress = "gz"
)

# > p_actv ~ sexo + raca  ----

df4e <- survey::svyby(
  ~ P040_todo_trajeto
  , by = ~  raca_group + sexo
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df4e

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_raca.rds"
  , x = list("sexo_raca" = df4e)
  , compress = "gz"
)

# > p_actv ~ sexo + escolaridade ----

options(survey.lonely.psu = "adjust") 

df4c <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ edugroup + sexo
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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

# > p_actv ~ raca + escolaridade ----

options(survey.lonely.psu = "adjust") 

df4c <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ edugroup + raca_group
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df4c

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/raca_esc.rds"
  ,x = list(
    "raca_esc" = df4c
  )
  ,compress = "gz"
)
# > p_actv ~ sexo + raca + escolaridade ----

#options(survey.lonely.psu = "adjust") 
#
#df4c <- survey::svyby(
#  formula = ~ P040_todo_trajeto
#  , by = ~ edugroup + sexo + raca_group
#  , design = sample_pns_pos_urban
#  , vartype = "ci"
#  , ci = TRUE
#  , level = 0.95
#  , FUN = svyciprop
#  , multicore = getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#
#df4c
#
#readr::write_rds(
#  file = "../../data/transporte_ativo_2008-2019/export_pns13/sexo_raca_esc.rds"
#  ,x = list(
#    "sexo_raca_esc" = df4c
#  )
#  ,compress = "gz"
#)
#
# > p_actv ~ regiao + quintileRegion  ----

df5b <- survey::svyby(
  formula = ~P040_todo_trajeto
  , by = ~ region + quintileRegion
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
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
  , x = list("region_quint_1" = df5b)
  , compress = "gz"
) 

# > p_actv ~ metro + quintileMetro  ----

df5b <- survey::svyby(
  formula = ~P040_todo_trajeto
  , by = ~ metro + quintileMetro
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

df5b

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13/metro_quint.rds"
  ,x = list("metro_quint_1" = df5b)
  ,compress = "gz"
) 


# > p_actv ~ BR + quintileBr  ----

df5d <- survey::svyby(
  formula = ~P040_todo_trajeto
  , by = ~ country + quintileBR
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
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

## > time_xx_to_yy  ----

options(survey.lonely.psu = "adjust") 

time_00to09 <- survey::svyby(
  formula = ~actv_commutetime_00to09
  , by = ~ sexo
  , design = pos_todo_parte_traj
  , FUN = svyciprop
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_00to09

time_10to19 <- survey::svyby(
  formula = ~actv_commutetime_10to19
  , by = ~ sexo
  , design = pos_todo_parte_traj
  , FUN = svyciprop
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_10to19

time_20to29 <- survey::svyby(
  formula = ~actv_commutetime_20to29
  , by = ~ sexo
  , design = pos_todo_parte_traj
  , FUN = svyciprop
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_20to29

time_30to44 <- survey::svyby(
  formula = ~actv_commutetime_30to44
  , by = ~ sexo
  , design = pos_todo_parte_traj
  , FUN = svyciprop
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , multicore=getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)

time_30to44

time_45to59 <- survey::svyby(
  formula = ~actv_commutetime_45to59
  , by = ~ sexo
  , design = pos_todo_parte_traj
  , FUN = svyciprop
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , multicore = getOption("survey.multicore")
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
