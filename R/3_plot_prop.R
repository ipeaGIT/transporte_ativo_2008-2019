# load and read -----

rm(list=ls())
gc(reset=T)
library(ggplot2)
library(readr)
library(data.table)
library(magrittr)
library(patchwork)

# 1) prop ~ br + metro + sit ----------

# read
pnad2008_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/br.rds")
pns13_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/br.rds")
pns19_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/br.rds")

pnad2008_dummyMetro <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/dummyMetro.rds")
pns13_dummyMetro <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/dummyMetro.rds")
pns19_dummyMetro <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/dummyMetro.rds")

pnad2008_sitbr <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/sit_brasil.rds")
pns13_sitbr <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/sit_brasil.rds")
pns19_sitbr <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/sit_brasil.rds")


# list
pnad2008_br <- pnad2008_br$brasil
pnad2008_dummyMetro <- pnad2008_dummyMetro$dummyMetro
pnad2008_sitbr <- pnad2008_sitbr$sit_brasil


# to DT
data.table::setDT(pnad2008_br)
data.table::setDT(pnad2008_dummyMetro)
data.table::setDT(pnad2008_sitbr)

data.table::setDT(pns13_br$brasil_1)
data.table::setDT(pns13_br$brasil_2)
data.table::setDT(pns19_br$brasil_1)
data.table::setDT(pns19_br$brasil_2)

data.table::setDT(pns13_dummyMetro$dummyMetro_1)
data.table::setDT(pns13_dummyMetro$dummyMetro_2)
data.table::setDT(pns19_dummyMetro$dummyMetro_1)
data.table::setDT(pns19_dummyMetro$dummyMetro_2)

data.table::setDT(pns13_sitbr$sit_brasil_1)
data.table::setDT(pns13_sitbr$sit_brasil_2)
data.table::setDT(pns19_sitbr$sit_brasil_1)
data.table::setDT(pns19_sitbr$sit_brasil_2)

# add/remove columns
pnad2008_sitbr[,country := NULL]
pns13_sitbr$sit_brasil_1[,country := NULL]
pns13_sitbr$sit_brasil_2[,country := NULL]
pns19_sitbr$sit_brasil_1[,country := NULL]
pns19_sitbr$sit_brasil_2[,country := NULL]

# rename
vec_names <- c("region","mean","ci_l","ci_u")
names(pnad2008_br) <- vec_names
names(pnad2008_sitbr) <- vec_names
names(pnad2008_dummyMetro) <- vec_names

names(pns13_dummyMetro$dummyMetro_1) <- vec_names
names(pns13_dummyMetro$dummyMetro_2) <- vec_names
names(pns19_dummyMetro$dummyMetro_1) <- vec_names
names(pns19_dummyMetro$dummyMetro_2) <- vec_names

names(pns13_br$brasil_1) <- vec_names
names(pns13_br$brasil_2) <- vec_names
names(pns19_br$brasil_1) <- vec_names
names(pns19_br$brasil_2) <- vec_names

names(pns13_sitbr$sit_brasil_1) <- vec_names
names(pns13_sitbr$sit_brasil_2) <- vec_names
names(pns19_sitbr$sit_brasil_1) <- vec_names
names(pns19_sitbr$sit_brasil_2) <- vec_names

# add remove columns

pnad2008_br[,":="(type = "Sim",ano = 2008)]
pnad2008_sitbr[,":="(type = "Sim",ano = 2008)]
pnad2008_dummyMetro[,":="(type = "Sim",ano = 2008)]


pns13_dummyMetro$dummyMetro_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13_dummyMetro$dummyMetro_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19_dummyMetro$dummyMetro_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19_dummyMetro$dummyMetro_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]


pns13_br$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13_br$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19_br$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19_br$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]

pns13_sitbr$sit_brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13_sitbr$sit_brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19_sitbr$sit_brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19_sitbr$sit_brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]

single_dt2 <- list(
  # Br files
  pnad2008_br
  ,pns13_br$brasil_1
  ,pns13_br$brasil_2
  ,pns19_br$brasil_1
  ,pns19_br$brasil_2
  # situacao files
  ,pnad2008_sitbr
  ,pns13_sitbr$sit_brasil_1
  ,pns13_sitbr$sit_brasil_2
  ,pns19_sitbr$sit_brasil_1
  ,pns19_sitbr$sit_brasil_2
  # metro files
  ,pnad2008_dummyMetro
  ,pns13_dummyMetro$dummyMetro_1
  ,pns13_dummyMetro$dummyMetro_2
  ,pns19_dummyMetro$dummyMetro_1
  ,pns19_dummyMetro$dummyMetro_2
  
) %>% data.table::rbindlist(use.names = TRUE)


# fix factors


single_dt2[,type := factor(x = type
                           ,levels = c("Sim"
                                       ,"Sim, parte do trajeto"
                                       ,"Sim, todo o trajeto"
                           ))]


single_dt2[,":="(
  ci_l1 = fifelse(type == "Sim, todo o trajeto",ci_l,ci_l+mean[2])
  ,ci_u1 = fifelse(type == "Sim, todo o trajeto",ci_u,ci_u+mean[2])
)
,by = .(ano,region)
]

single_dt2[is.na(ci_l1),":="(
  ci_l1 = ci_l ,
  ci_u1 = ci_u 
)]

single_dt2[,region_f := factor(
  x = region
  ,levels = c("Brasil","Urbano","Rural","Metro","Non-metro")
  ,labels = c("Brasil","Brasil Urbano","Brasil Rural","Metropolitano","Não metropolitano")
)]

# plot
ggplot(data = single_dt2
       , aes(x = factor(ano)
             , y = mean
             , fill = type)) + 
  geom_bar(stat="identity"
           , color="black"
           , width = .75
  ) +
  scale_y_continuous(labels = scales::percent)+
  facet_grid(~region_f)+
  scale_fill_brewer(palette = "Spectral") +
  geom_errorbar(aes(
    ymin = ci_l1
    , ymax = ci_u1)
    , width=.3
  ) +
  labs(
    title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Pessoas acima de 18 anos"
    , x = NULL, y = "Proporção (%)"
    , fill = "Tipo de \nresposta"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  theme_classic()+
  theme(legend.position = "bottom")


ggsave(filename = "figures/prop_metro_sit_brasil.png"
       ,width = 15
       ,height = 10
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)

# 3) prop ~ BR + sexo-----

rm(list=ls())

# read files
pnad2008_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/br.rds")
pns13_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/br.rds")
pns19_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/br.rds")

pnad2008_sexo <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/sexo.rds")
pns13_sexo <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/sexo.rds")
pns19_sexo <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/sexo.rds")

# to DT
data.table::setDT(pnad2008_br$brasil)
data.table::setDT(pns13_br$brasil_1)
data.table::setDT(pns13_br$brasil_2)
data.table::setDT(pns19_br$brasil_1)
data.table::setDT(pns19_br$brasil_2)
data.table::setDT(pnad2008_sexo$sexo)
data.table::setDT(pns13_sexo$sexo_1)
data.table::setDT(pns13_sexo$sexo_2)
data.table::setDT(pns19_sexo$sexo_1)
data.table::setDT(pns19_sexo$sexo_2)

# rename columns
vec_names <- c("sexo","mean","ci_l","ci_u")
names(pnad2008_br$brasil) <- vec_names
names(pns13_br$brasil_1) <- vec_names
names(pns13_br$brasil_2) <- vec_names
names(pns19_br$brasil_1) <- vec_names
names(pns19_br$brasil_2) <- vec_names

names(pnad2008_sexo$sexo) <- vec_names
names(pns13_sexo$sexo_1) <- vec_names
names(pns13_sexo$sexo_2) <- vec_names
names(pns19_sexo$sexo_1) <- vec_names
names(pns19_sexo$sexo_2) <- vec_names

# add columns
pnad2008_br$brasil[,":="(type = "Sim",ano = 2008)]
pns13_br$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13_br$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19_br$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19_br$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]


pnad2008_sexo$sexo[,":="(type = "Sim",ano = 2008)]
pns13_sexo$sexo_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13_sexo$sexo_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19_sexo$sexo_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19_sexo$sexo_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]

# rbind
single_dt3 <- list(
  # Br files
  pnad2008_br$brasil
  ,pns13_br$brasil_1
  ,pns13_br$brasil_2
  ,pns19_br$brasil_1
  ,pns19_br$brasil_2
  # sexo files
  ,pnad2008_sexo$sexo
  ,   pns13_sexo$sexo_1
  ,   pns13_sexo$sexo_2
  ,   pns19_sexo$sexo_1
  ,   pns19_sexo$sexo_2
  
) %>% data.table::rbindlist()

# fix factors
single_dt3[,type := factor(x = type
                           ,levels = c("Sim"
                                       ,"Sim, parte do trajeto"
                                       ,"Sim, todo o trajeto"
                           ))]


single_dt3[,":="(
  ci_l1 = fifelse(type == "Sim, todo o trajeto",ci_l,ci_l+mean[2])
  ,ci_u1 = fifelse(type == "Sim, todo o trajeto",ci_u,ci_u+mean[2])
)
,by = .(ano,sexo)
]


single_dt3[is.na(ci_l1),":="(ci_l1 = ci_l , ci_u1 = ci_u)]

# plot
ggplot(data = single_dt3
       , aes(x = factor(ano)
             , y = mean
             , fill = type)) + 
  geom_bar(stat="identity"
           , color="black",alpha = 0.5
  ) +
  facet_grid(~sexo)+
  scale_fill_brewer(palette = "Set1") +
  geom_errorbar(aes(
    ymin = ci_l1
    , ymax = ci_u1)
    , width=.2
  ) +
  labs(
    title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Pessoas acima de 18 anos"
    , x = NULL, y = "Proporção (%)"
    , fill = "Tipo de \nresposta"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  theme_classic()


ggsave(filename = "figures/prop_sexo_brasil.png"
       ,width = 18
       ,height = 10.8
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)

# end----