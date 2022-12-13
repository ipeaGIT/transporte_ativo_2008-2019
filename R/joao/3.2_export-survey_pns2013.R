# Create survey design for PNS 2019 data 
# 1) Load packages  ------
rm(list=ls())
gc(reset=TRUE)
library(survey)
library(srvyr)
library(data.table)
library(magrittr)

# 2) read ------
pns13_raw <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2013_design.rds")

#pns13_dt <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns132008.rds")
# pns13_dt[,table(v1410),by = .(metro,urban)]
# pns13_dt[,.SD[1],by = .(metro,urban)]
# pns13_dt[metro == "Rio de Janeiro" & urban == "Rural",v1410] %>% table(exclude = F)
# pns13_dt[metro == "Belém" & urban == "Rural",v1410] %>% table(exclude = F)
# pns13_dt[metro == "Belo Horizonte" & urban == "Rural",v1410] %>% table(exclude = F)
# pns13_dt[metro == "Curitiba" & urban == "Urban",v1410] %>% table(exclude = F)
# pns13_dt[metro == "Salvador" & urban == "Urban",v1410] %>% table(exclude = F)
# pns13_dt[metro == "Distrito Federal" & urban == "Urban",v1410] %>% table(exclude = F)
# pns13_dt[metro == "Belém" & urban == "Rural",v1410] %>% table(exclude = F)
# pns13_dt[metro == "Rio de Janeiro" & urban == "Urban",v1410] %>% table(exclude = F)
# df1
# pns13_raw <- readr::read_rds("../../data/transporte_ativo_2008-2019/sample_pns2013_pos.rds")
# pns19_raw <- readr::read_rds("../../data/transporte_ativo_2008-2019/sample_pns2019_pos.rds")
#pns13_dt <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2013.rds")
#pns13_dt[metro == "Porto Alegre" & urban == "Rural",v1410] %>% table(exclude = F)

# 3) filter------ 
pns13_18y <- subset(pns13_raw, C008 >= 18)
#pns13_18y <- subset(pns13_raw, C008 >= 18)
#pns13_18y <- subset(pns19_raw, C008 >= 18)


#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing 
# - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu 

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
# df1b
# > p_actv_commute ~ brazil ----

future::plan("multisession",workers  = 20)
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

# > p_actv_commute ~ brazil + situacao----

future::plan("multisession",workers  = 20)
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
df2d

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

# metro factor(v1410 == "Sim")      ci_l      ci_u
# Belém                       Belém              0.2539414 0.2354109 0.2734088
# Belo Horizonte     Belo Horizonte              0.2126927 0.1964941 0.2298447
# Curitiba                 Curitiba              0.0000000 0.0000000 0.0000000
# Distrito Federal Distrito Federal              0.0000000 0.0000000 0.0000000
# Fortaleza               Fortaleza              0.2764896 0.2598727 0.2937473
# Porto Alegre         Porto Alegre              0.2096792 0.1968890 0.2230695
# Recife                     Recife              0.2618165 0.2442832 0.2801418
# Restante das UF   Restante das UF              0.3934237 0.3853508 0.4015552
# Rio de Janeiro     Rio de Janeiro              0.1590369 0.1478397 0.1709120
# Salvador                 Salvador              0.2033512 0.1885990 0.2189460
# São Paulo               São Paulo              0.1529182 0.1427324 0.1636921


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
df4b

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


# 
# > p_actv_commute ~ regiao + quintileRegion  ----

# options(survey.lonely.psu = "adjust") 
# 
# df5a <- survey::svyby(
#   formula = ~factor(v1410 == "Sim, parte do trajeto")
#   , by = ~ region + quintileRegion
#   , design = pns13_18y
#   , vartype = "ci", ci = TRUE
#   , level = 0.95
#   , FUN = svyciprop
#   , multicore = getOption("survey.multicore")
#   , verbose = TRUE
#   , na.rm.all = TRUE
#   , drop.empty.groups = TRUE
# )
# df5b <- survey::svyby(
#   formula = ~factor(v1410 == "Sim, todo o trajeto")
#   , by = ~ region + quintileRegion
#   , design = pns13_18y
#   , vartype = "ci", ci = TRUE
#   , level = 0.95
#   , FUN = svyciprop
#   , multicore = getOption("survey.multicore")
#   , verbose = TRUE
#   , na.rm.all = TRUE
#   , drop.empty.groups = TRUE
# )
# 
# df5b
# 
# pns13_18y$variables[v1410 == "Sim",.N,by=.(region,quintileRegion)]
# 
# 
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
#
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

# Merge ----

df_time <- list(
  # "sit_region_1" = df1a
  #,"sit_region_2" = df1b
  "brasil_1" = df2a
  ,"brasil_2" = df2b
  ,"sit_brasil_1" = df2c
  ,"sit_brasil_2" = df2d
  ,"dummyMetro_1" = df3a
  ,"dummyMetro_2" = df3b
  ,"sexo_1" = df4a
  ,"sexo_2" = df4b
  #,"sexo_esc_1" = df4c
  #,"sexo_esc_2" = df4d
  #,"region_quint_1" = df5a
  #,"region_quint_2" = df5b
 # ,"sex_race_1" = df6a
 # ,"sex_race_2" = df6b
 # ,"time_00to09" = time_00to09
 # ,"time_10to19" = time_10to19
 # ,"time_20to29" = time_20to29
 # ,"time_30to44" = time_30to44
 # ,"time_45to59" = time_45to59
)

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pns13.rds"
  ,x = df_time
  ,compress = "gz"
)
