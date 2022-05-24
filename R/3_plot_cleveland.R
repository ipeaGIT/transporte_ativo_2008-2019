# load and read -----

rm(list=ls())
gc(reset=T)
library(ggplot2)
library(readr)
library(data.table)
library(magrittr)
library(patchwork)



# 1) prop_active_commute ~ br + metro ----------

pnad2008_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/br.rds")
   pns13_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/br.rds")
   pns19_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/br.rds")

pnad2008_dummyMetro <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/dummyMetro.rds")
pns13_dummyMetro <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/dummyMetro.rds")
pns19_dummyMetro <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/dummyMetro.rds")

data.table::setDT(pnad2008_br$brasil)
data.table::setDT(pns13_br$brasil_1)
data.table::setDT(pns13_br$brasil_2)
data.table::setDT(pns19_br$brasil_1)
data.table::setDT(pns19_br$brasil_2)
data.table::setDT(pnad2008_dummyMetro$dummyMetro)
data.table::setDT(pns13_dummyMetro$dummyMetro_1)
data.table::setDT(pns13_dummyMetro$dummyMetro_2)
data.table::setDT(pns19_dummyMetro$dummyMetro_1)
data.table::setDT(pns19_dummyMetro$dummyMetro_2)

vec_names <- c("country","mean","ci_l","ci_u")
names(pnad2008_dummyMetro$dummyMetro) <- vec_names
names(pns13_dummyMetro$dummyMetro_1) <- vec_names
names(pns13_dummyMetro$dummyMetro_2) <- vec_names
names(pns19_dummyMetro$dummyMetro_1) <- vec_names
names(pns19_dummyMetro$dummyMetro_2) <- vec_names
names(pnad2008_br$brasil) <- vec_names
names(pns13_br$brasil_1) <- vec_names
names(pns13_br$brasil_2) <- vec_names
names(pns19_br$brasil_1) <- vec_names
names(pns19_br$brasil_2) <- vec_names

pnad2008_dummyMetro$dummyMetro[,":="(type = "Sim",ano = 2008)]
pns13_dummyMetro$dummyMetro_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13_dummyMetro$dummyMetro_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19_dummyMetro$dummyMetro_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19_dummyMetro$dummyMetro_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]
pnad2008_br$brasil[,":="(type = "Sim",ano = 2008)]
pns13_br$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13_br$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19_br$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19_br$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]

single_dt2 <- list(
  # Br files
  pnad2008_br$brasil
  ,pns13_br$brasil_1
  ,pns13_br$brasil_2
  ,pns19_br$brasil_1
  ,pns19_br$brasil_2
  # metro files
  ,pnad2008_dummyMetro$dummyMetro
  ,pns13_dummyMetro$dummyMetro_1
  ,pns13_dummyMetro$dummyMetro_2
  ,pns19_dummyMetro$dummyMetro_1
  ,pns19_dummyMetro$dummyMetro_2
  
) %>% data.table::rbindlist()

single_dt2[,type := factor(x = type
                           ,levels = c("Sim"
                                       ,"Sim, parte do trajeto"
                                       ,"Sim, todo o trajeto"
                           ))]
#single_dt2 <- single_dt2[country != "Non-metro"]
single_dt2[,":="(
  ci_l1 = fifelse(type == "Sim, todo o trajeto",ci_l,ci_l+mean[2])
  ,ci_u1 = fifelse(type == "Sim, todo o trajeto",ci_u,ci_u+mean[2])
)
,by = .(ano,country)
]
single_dt2[is.na(ci_l1),":="(
  ci_l1 = ci_l ,
  ci_u1 = ci_u 
)]

ggplot(data = single_dt2
       , aes(x = factor(ano)
             , y = mean
             , fill = type)) + 
  geom_bar(stat="identity"
           , color="black"
  ) +
  facet_grid(~~country)+
  scale_fill_brewer(palette = "Spectral") +
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


ggsave(filename = "figures/prop_metro_brasil.png"
       ,width = 18
       ,height = 10.8
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)
