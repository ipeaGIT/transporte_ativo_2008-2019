# load and read -----

rm(list=ls())
gc(reset=T)
library(ggplot2)
library(readr)
library(data.table)
library(magrittr)
library(patchwork)


pnad2008 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad2008.rds")
pns13 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13.rds")
pns19 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19.rds")


# 1) prop_active_commute ~ br + metro ----------
data.table::setDT(pnad2008$brasil)
data.table::setDT(pns13$brasil_1)
data.table::setDT(pns13$brasil_2)
data.table::setDT(pns19$brasil_1)
data.table::setDT(pns19$brasil_2)
data.table::setDT(pnad2008$dummyMetro)
data.table::setDT(pns13$dummyMetro_1)
data.table::setDT(pns13$dummyMetro_2)
data.table::setDT(pns19$dummyMetro_1)
data.table::setDT(pns19$dummyMetro_2)

vec_names <- c("country","mean","ci_l","ci_u")
names(pnad2008$dummyMetro) <- vec_names
names(pns13$dummyMetro_1) <- vec_names
names(pns13$dummyMetro_2) <- vec_names
names(pns19$dummyMetro_1) <- vec_names
names(pns19$dummyMetro_2) <- vec_names
names(pnad2008$brasil) <- vec_names
names(pns13$brasil_1) <- vec_names
names(pns13$brasil_2) <- vec_names
names(pns19$brasil_1) <- vec_names
names(pns19$brasil_2) <- vec_names

pnad2008$dummyMetro[,":="(type = "Sim",ano = 2008)]
pns13$dummyMetro_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13$dummyMetro_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19$dummyMetro_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19$dummyMetro_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]
pnad2008$brasil[,":="(type = "Sim",ano = 2008)]
pns13$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]

single_dt2 <- list(
  # Br files
  pnad2008$brasil
  ,pns13$brasil_1
  ,pns13$brasil_2
  ,pns19$brasil_1
  ,pns19$brasil_2
  # metro files
  ,pnad2008$dummyMetro
  ,pns13$dummyMetro_1
  ,pns13$dummyMetro_2
  ,pns19$dummyMetro_1
  ,pns19$dummyMetro_2
  
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

# 2) prop_active_commute ~ BR + sit-----
data.table::setDT(pnad2008$sit_brasil)
data.table::setDT(pns13$sit_brasil_1)
data.table::setDT(pns13$sit_brasil_2)
data.table::setDT(pns19$sit_brasil_1)
data.table::setDT(pns19$sit_brasil_2)

vec_names <- c("country","urban","mean","ci_l","ci_u")
names(pnad2008$sit_brasil) <- vec_names
names(pns13$sit_brasil_1) <- vec_names
names(pns13$sit_brasil_2) <- vec_names
names(pns19$sit_brasil_1) <- vec_names
names(pns19$sit_brasil_2) <- vec_names

pns13$brasil_1[,":="(urban = c("Geral"))]
pns13$brasil_2[,":="(urban = c("Geral"))]
pns19$brasil_1[,":="(urban = c("Geral"))]
pns19$brasil_2[,":="(urban = c("Geral"))]
pnad2008$brasil[,":="(urban = c("Geral"))]
pnad2008$sit_brasil[,":="(type = "Sim",ano = 2008,urban = c("Rural","Urbano"))]
pns13$sit_brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
pns13$sit_brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
pns19$sit_brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
pns19$sit_brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]

single_dt <- list(
   pnad2008$brasil
  ,pns13$brasil_1
  ,pns13$brasil_2
  ,pns19$brasil_1
  ,pns19$brasil_2
  ,pnad2008$sit_brasil
  ,pns13$sit_brasil_1
  ,pns13$sit_brasil_2
  ,pns19$sit_brasil_1
  ,pns19$sit_brasil_2
) %>% data.table::rbindlist(use.names = T)

single_dt[,type := factor(x = type
                          ,levels = c("Sim"
                                      ,"Sim, parte do trajeto"
                                      ,"Sim, todo o trajeto"
                          )
                          ,)]
single_dt[,":="(
  ci_l1 = fifelse(type == "Sim, todo o trajeto",ci_l,ci_l+mean[2])
  ,ci_u1 = fifelse(type == "Sim, todo o trajeto",ci_u,ci_u+mean[2])
)
,by = .(ano,urban)
]
single_dt[is.na(ci_l1),":="(
  ci_l1 = ci_l ,
  ci_u1 = ci_u 
)]
single_dt[,urban := factor(x = urban
                          ,levels = c("Geral"
                                      ,"Urbano"
                                      ,"Rural"
                          )
                          ,)]
ggplot(data = single_dt
       , aes(x = factor(ano)
             , y = mean
             , fill = type)) + 
  geom_bar(stat="identity"
           , color="black"
  ) +
  facet_grid(~urban
             , labeller = as_labeller(c(`Geral` = "Brasil"
                                        ,`Urbano` = "Brasil Urbano"
                                        , `Rural` = "Brasil Rural")))+
  scale_fill_brewer(palette = "RdYlGn") +
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
ggsave(filename = "figures/prop_sit_brasil.png"
       ,width = 18
       ,height = 10.8
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)

# 2) prop_active_commute ~ BR + sexo-----
data.table::setDT(pnad2008$brasil)
data.table::setDT(pns13$brasil_1)
data.table::setDT(pns13$brasil_2)
data.table::setDT(pns19$brasil_1)
data.table::setDT(pns19$brasil_2)
data.table::setDT(pnad2008$sexo)
   data.table::setDT(pns13$sexo_1)
   data.table::setDT(pns13$sexo_2)
   data.table::setDT(pns19$sexo_1)
   data.table::setDT(pns19$sexo_2)

vec_names <- c("sexo","mean","ci_l","ci_u")
names(pnad2008$brasil) <- vec_names
names(pns13$brasil_1) <- vec_names
names(pns13$brasil_2) <- vec_names
names(pns19$brasil_1) <- vec_names
names(pns19$brasil_2) <- vec_names
names(pnad2008$sexo) <- vec_names
names(pns13$sexo_1) <- vec_names
names(pns13$sexo_2) <- vec_names
names(pns19$sexo_1) <- vec_names
names(pns19$sexo_2) <- vec_names

pnad2008$brasil[,":="(type = "Sim",ano = 2008)]
 pns13$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
 pns13$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
 pns19$brasil_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
 pns19$brasil_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]
  pnad2008$sexo[,":="(type = "Sim",ano = 2008)]
   pns13$sexo_1[,":="(type = "Sim, parte do trajeto",ano = 2013)]
   pns13$sexo_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2013)]
   pns19$sexo_1[,":="(type = "Sim, parte do trajeto",ano = 2019)]
   pns19$sexo_2[,":="(type = "Sim, todo o trajeto"  ,ano = 2019)]


single_dt3 <- list(
  # Br files
  pnad2008$brasil
  ,pns13$brasil_1
  ,pns13$brasil_2
  ,pns19$brasil_1
  ,pns19$brasil_2
  # metro files
  ,pnad2008$sexo
  ,   pns13$sexo_1
  ,   pns13$sexo_2
  ,   pns19$sexo_1
  ,   pns19$sexo_2
  
) %>% data.table::rbindlist()

single_dt3[,type := factor(x = type
                           ,levels = c("Sim"
                                       ,"Sim, parte do trajeto"
                                       ,"Sim, todo o trajeto"
                           ))]
#single_dt2 <- single_dt2[country != "Non-metro"]
single_dt3[,":="(
  ci_l1 = fifelse(type == "Sim, todo o trajeto",ci_l,ci_l+mean[2])
  ,ci_u1 = fifelse(type == "Sim, todo o trajeto",ci_u,ci_u+mean[2])
)
,by = .(ano,sexo)
]
single_dt3[is.na(ci_l1),":="(
  ci_l1 = ci_l ,
  ci_u1 = ci_u 
)]

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