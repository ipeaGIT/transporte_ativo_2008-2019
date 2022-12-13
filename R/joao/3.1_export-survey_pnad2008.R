# Create survey design for PNS 2019 data 
# 1) Load packages  ------
rm(list=ls())
gc(reset=TRUE)
library(survey)
library(srvyr)
library(data.table)
library(magrittr)

# 2) read ------
pnad_raw <- readr::read_rds("../../data/transporte_ativo_2008-2019/sample_pnad2008_pos.rds")
#pnad_dt <- readr::read_rds("../../data/transporte_ativo_2008-2019/pnad2008.rds")
# pnad_dt[,table(v1410),by = .(metro,urban)]
# pnad_dt[,.SD[1],by = .(metro,urban)]
# pnad_dt[metro == "Rio de Janeiro" & urban == "Rural",v1410] %>% table(exclude = F)
# pnad_dt[metro == "Belém" & urban == "Rural",v1410] %>% table(exclude = F)
# pnad_dt[metro == "Belo Horizonte" & urban == "Rural",v1410] %>% table(exclude = F)
# pnad_dt[metro == "Curitiba" & urban == "Urban",v1410] %>% table(exclude = F)
# pnad_dt[metro == "Salvador" & urban == "Urban",v1410] %>% table(exclude = F)
# pnad_dt[metro == "Distrito Federal" & urban == "Urban",v1410] %>% table(exclude = F)
# pnad_dt[metro == "Belém" & urban == "Rural",v1410] %>% table(exclude = F)
# pnad_dt[metro == "Rio de Janeiro" & urban == "Urban",v1410] %>% table(exclude = F)
# df1
# pns13_raw <- readr::read_rds("../../data/transporte_ativo_2008-2019/sample_pns2013_pos.rds")
# pns19_raw <- readr::read_rds("../../data/transporte_ativo_2008-2019/sample_pns2019_pos.rds")
#pns13_dt <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2013.rds")
#pns13_dt[metro == "Porto Alegre" & urban == "Rural",v1410] %>% table(exclude = F)

# 3) filter------ 
pnad_18y <- subset(pnad_raw, v8005 >= 18)
#pns13_18y <- subset(pns13_raw, C008 >= 18)
#pns19_18y <- subset(pns19_raw, C008 >= 18)


#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing 
# - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu 

# > p_actv_commute ~ situacao + region ----

#future::plan("multisession",workers  = 20)
#df1 <- survey::svyby(~ factor(v1410), ~ region + urban
#                    , design = pnad_18y
#                    , vartype = "ci", ci = TRUE
#                    , level = 0.95, FUN = svyciprop
#                    #, method = "likelihood"
#                    , multicore = getOption("survey.multicore"),
#                    , verbose = TRUE
#                    , na.rm.all = FALSE
#                    , drop.empty.groups = FALSE)
#df1

# > p_actv_commute ~ brazil ----

future::plan("multisession",workers  = 20)
df2 <- survey::svyby(
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
df2

# > p_actv_commute ~ brazil + situacao ----

df2a <- survey::svyby(
  ~ factor(v1410 == "Sim"), ~ country + urban
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

# > p_actv_commute ~ dummyMetro----

df3 <- survey::svyby(
  formula = ~factor(v1410 == "Sim")
  , by = ~ dummyMetro 
  , design = pnad_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  #, method = "likelihood"
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = FALSE
  , drop.empty.groups = FALSE
)

df3

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


# > p_actv_commute ~ sexo + escolaridade ----

#options(survey.lonely.psu = "adjust") 
#
#df4 <- survey::svyby(
#  formula = ~factor(v1410 == "Sim")
#  , by = ~ edugroup + sexo
#  , design = pnad_18y
#  , vartype = "ci", ci = TRUE
#  , level = 0.95, FUN = svyciprop
#  , multicore = getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)
#df4

# > p_actv_commute ~ sexo  ----

options(survey.lonely.psu = "adjust") 

df4a <- survey::svyby(
  formula = ~factor(v1410 == "Sim")
  , by = ~ sexo
  , design = pnad_18y
  , vartype = "ci", ci = TRUE
  , level = 0.95, FUN = svyciprop
  , multicore = getOption("survey.multicore")
  , verbose = TRUE
  , na.rm.all = TRUE
  , drop.empty.groups = TRUE
)
df4a

pnad_18y$variables[v1410 == "Sim",.N,by=.(edugroup,sexo)]


# 
# > p_actv_commute ~ regiao + quintileRegion  ----

#options(survey.lonely.psu = "adjust") 
#
#df5 <- survey::svyby(
#  formula = ~factor(v1410 == "Sim")
#  , by = ~ region + quintileRegion
#  , design = pnad_18y
#  , vartype = "ci", ci = TRUE
#  , level = 0.95
#  , FUN = svyciprop
#  , multicore = getOption("survey.multicore")
#  , verbose = TRUE
#  , na.rm.all = TRUE
#  , drop.empty.groups = TRUE
#)


pnad_18y$variables[v1410 == "Sim",.N,by=.(region,quintileRegion)]

# 
# > p_actv_commute ~ sexo + raca  ----

#options(survey.lonely.psu = "adjust") 
#
#df6 <- survey::svyby(formula = ~factor(v1410 == "Sim")
#                     , by = ~ sexo + raca_group
#                     , design = pnad_18y
#                     #, design = subset(pnad_18y,region == "Sul") 
#                     , vartype = "ci", ci = TRUE
#                     , level = 0.95
#                     , FUN = svyciprop
#                     #, multicore = TRUE,
#                     , verbose = TRUE
#                     , na.rm.all = TRUE
#                     , drop.empty.groups = TRUE)
#df6
pnad_18y$variables[v1410 == "Sim",.N,by=.(region,quintileRegion)]

# > time_xx_to_yy  ----

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

# Merge ----
df_time <- list(
    #"sit_region" = df1
   "brasil" = df2
  , "sit_brasil" = df2a
  , "dummyMetro" = df3
  #, "sexo_esc" = df4
  , "sexo" = df4a
  #, "region_quintil" = df5
  #, "time_00to09" = time_00to09
  #, "time_10to19" = time_10to19
  #, "time_20to29" = time_20to29
  #, "time_30to44" = time_30to44
  #, "time_45to59" = time_45to59
)

readr::write_rds(
  file = "../../data/transporte_ativo_2008-2019/export_pnad2008.rds"
  ,x = df_time
  ,compress = "gz"
)
