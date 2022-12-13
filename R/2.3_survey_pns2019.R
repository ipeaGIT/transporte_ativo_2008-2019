# Create survey design for PNS 2019 data 
# 1) Load packages  ------
rm(list = ls())
gc(reset = TRUE)
library(survey)
library(data.table)
library(magrittr)
# 2) Read files -----

pns2019_dt <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2019_dt.rds")

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


#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    


# 3) Design -----                    

#pns2019_dt <- pns2019_dt[M001 == 1,]
pns2019_dt[,V00293 := as.numeric(V00293)]
pns2019_dt[,V0015 := as.numeric(V0015)]
pns2019_dt[,V00291 := as.numeric(V00291)]
pns2019_dt[,V00292 := as.numeric(V00292)]
pns2019_dt <- pns2019_dt[!is.na(peso_morador_selec)]
pns2019_dt <- pns2019_dt[V0025A == "1"]


pre_stratified <- survey::svydesign(data = pns2019_dt
                                    , strata = ~V0024
                                    , id = ~UPA_PNS    # PSU
                                    , weights = ~peso_morador_selec 
                                    , nest = TRUE)

# filtro
pres_total <- pre_stratified %>% 
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
# 5) Export values -----
# > p_actv ~ situacao + region ----

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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sit_region.rds"
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
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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
  , design = pos_total
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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
  ~ P040_parte_trajeto
  , ~ dummyMetro
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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
  ~ P040_parte_trajeto
  , ~ dummyMetro + quintileDummyMetro
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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
  , vartype = "ci"
  , ci = TRUE
  , level = 0.95
  , FUN = svyciprop
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
  formula = ~ P040_todo_trajeto
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

df4b

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo.rds"
  ,x = list(
    "sexo_1" = df4a
    ,"sexo_2" = df4b
  )
  ,compress = "gz"
)

# > p_actv ~ sexo + age  ----

options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ agegroup + sexo
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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo_age.rds"
  ,x = list("sexo_age" = df4d )
  ,compress = "gz"
)

# > p_actv ~ sexo + ageLarge  ----

options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ AGE  + sexo
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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo_ageLarge.rds"
  ,x = list("sexo_age" = df4d )
  ,compress = "gz"
)
# > p_actv ~ sexo + escolaridade ----

options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
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


summary(pns2019_dt$peso_morador_selec)
setDT(df4d)
tmp <- pns2019_dt[V0025A == "1" & 
                    urban == "Urbano" &
                    !is.na(peso_morador_selec) &
                    !is.na(P040) &
                    C008 >= 18
        ,{
          # ww <- V00291
          # ww <- V00291 * peso_morador_selec
          ww <- peso_morador_selec
         total <-  sum(ww,na.rm = TRUE)
         total_ativ <-  sum(ww[P040 == "Sim, todo o trajeto"],na.rm = TRUE)
         prop <- round(100 * total_ativ / total,1)
         list(total,total_ativ,prop)
        }
        ,by = .(edugroup,sexo)]
tmp[]
tmp[] %>% .[c(1,2,3,7,8,4,5,6),]
df4d[]
#tmp[] %>% .[c(1,2,3,8,4,5,6,7),]

df4d[]
df4d[,sum(P040_todo_trajeto),by = edugroup]



readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo_esc.rds"
  ,x = list("sexo_esc" = df4d )
  ,compress = "gz"
)

# > p_actv ~ raca + escolaridade ----

options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ edugroup + raca_group
  , design =  pos_urbano
  #, design =  subset(pos_urbano,dummyMetro  == "Metro")  
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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/raca_esc.rds"
  ,x = list("raca_esc" = df4d )
  ,compress = "gz"
)
# > p_actv ~ sexo + quintil + Metro ----
options(survey.lonely.psu = "adjust") 

df4d <- survey::svyby(
  formula = ~ P040_todo_trajeto
  , by = ~ metro + quintileMetro + sexo
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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/sexo_metro_quint.rds"
  ,x = df4d
  ,compress = "gz"
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
  file = "../../data/transporte_ativo_2008-2019/export_pns19/metro_quint.rds"
  ,x = list(
    "region_quint_1" = df5b
  )
  ,compress = "gz"
) 

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
  , design = pos_urbano
  , vartype = "ci"
  , ci = TRUE
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
