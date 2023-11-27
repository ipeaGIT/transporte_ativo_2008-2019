# load and read -----
rm(list=ls()) 
data_path <- "../../data/transporte_ativo_2008-2019/" 
data_path <- "data/"
gc(reset=T)
library(ggplot2)
library(readr)
library(data.table)
library(magrittr)
library(patchwork)
library(openxlsx)

# 1) prop ~ br + metro + sit ----------
rm(list=ls()) 
data_path <- "../../data/transporte_ativo_2008-2019/" 
data_path <- "data/"
gc(reset=T)
# read
pnad2008_br <- readr::read_rds(paste0(data_path,"export_pnad08/br.rds"))
pns13_br <- readr::read_rds(paste0(data_path,"export_pns13/br.rds"))
pns19_br <- readr::read_rds(paste0(data_path,"export_pns19/br.rds"))

pnad2008_dummyMetro <- readr::read_rds(paste0(data_path,"export_pnad08/dummyMetro.rds"))
pns13_dummyMetro <- readr::read_rds(paste0(data_path,"export_pns13/dummyMetro.rds"))
pns19_dummyMetro <- readr::read_rds(paste0(data_path,"export_pns19/dummyMetro.rds"))

pnad2008_sitbr <- readr::read_rds(paste0(data_path,"export_pnad08/sit_brasil.rds"))
pns13_sitbr <- readr::read_rds(paste0(data_path,"export_pns13/sit_brasil.rds"))
pns19_sitbr <- readr::read_rds(paste0(data_path,"export_pns19/sit_brasil.rds"))


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

data.table::setDT(pns13_sitbr$sit_brasil_1)[urban == "Urbana",urban := "Urbano"]
data.table::setDT(pns13_sitbr$sit_brasil_2)[urban == "Urbana",urban := "Urbano"]
data.table::setDT(pns19_sitbr$sit_brasil_1)[urban == "Urbana",urban := "Urbano"]
data.table::setDT(pns19_sitbr$sit_brasil_2)[urban == "Urbana",urban := "Urbano"]

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
  ,labels = c("Brasil","Brasil Urbano","Brasil Rural"
              ,"Metropolitano \nUrbano","Não metropolitano \nUrbano")
)]


# text 
single_dt2[region == "Rural"]
single_dt2[region_f == "Metropolitano \nUrbano"]


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
  geom_errorbar(aes(
    ymin = ci_l1
    , ymax = ci_u1)
    , width=.3
  ) +
  labs(
    title = NULL
    , subtitle = NULL
    , x = NULL, y = "Proporção (%)"
    , fill = "Tipo de \nresposta"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  theme_classic()+
  # Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.key.width=unit(2,"line"),
        text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        legend.text = element_text(size = rel(0.8)
                                   , family = "Times New Roman"
                                   , face = "plain"),
        legend.title = element_text(size = rel(0.95)
                                    , family = "Times New Roman"
                                    , face = "bold"),
        title = element_text(size = 10
                             , family = "Times New Roman"
                             , face = "plain"),
        plot.margin=unit(c(0,2,0,1),"mm"),
        strip.text.x = element_text(size=rel(1.2)),
        panel.background = element_rect(fill = "white",colour = NA),
        )


ggsave(filename = "figures/prop_metro_sit_brasil.png"
       ,width = 15
       ,height = 10
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)

# 3) prop ~ BR + sexo-----

rm(list=ls()) 
data_path <- "../../data/transporte_ativo_2008-2019/" 
data_path <- "data/"

# read files
pnad2008_br <- readr::read_rds(paste0(data_path,"export_pnad08/br.rds"))
pns13_br <- readr::read_rds(paste0(data_path,"export_pns13/br.rds"))
pns19_br <- readr::read_rds(paste0(data_path,"export_pns19/br.rds"))

pnad2008_sexo <- readr::read_rds(paste0(data_path,"export_pnad08/sexo.rds"))
pns13_sexo <- readr::read_rds(paste0(data_path,"export_pns13/sexo.rds"))
pns19_sexo <- readr::read_rds(paste0(data_path,"export_pns19/sexo.rds"))

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
    # title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    title = NULL
    , subtitle = NULL
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

# 4) EXP - prop dia ativ hab. -----
rm(list=ls()) 
data_path <- "../../data/transporte_ativo_2008-2019/" 
data_path <- "data/"
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(patchwork)

pns19 <- readr::read_rds("data/pns2019_dt.rds")

# | P040     | Para ir ou voltar do trabalho, o(a) Sr(a) faz algum       |  both   |
# |          | trajeto a pé ou de bicicleta?                             |         |
# |          | 1  Sim, todo o trajeto ;                                  |         |
# |          | 2  Sim, parte do trajeto;                                 |         |
# |          | 3  Não; Não aplicável                                     |         |
# | P04001   | Quantos dias por semana o(a) Sr(a) faz algum trajeto a pé |   2019  |
# |          | ou bicicleta?                                             |         |
# | P04101   | numero de horas da ativ acima (ida e volta)               |   both  |
# | P04102   | numero de minut da ativ acima (ida e volta)               |   both  |
# | P042     | Nas suas atividades habituais (tais como ir, ou levar     |         |
# |          | alguém, a algum curso, escola ou clube), quantos dias por |         |
# |          | semana o(a) Sr(a) faz alguma atividade que envolva        |         |
# |          | deslocamento a pé ou bicicleta?                           |         |
# |          | (0 = Nunca ou menos de uma vez por semana)                |         |
# | P04301   | numero de horas da ativ acima (ida e volta)               |   both  |
# | P04302   | numero de minut da ativ acima (ida e volta)               |   both  |
# |   C006   | Sexo                                                      |   both  |
# |          | 1  Masculino                                              |         |
# |          | 2  Feminino                                               |         |
dic_sexo <- data.table(C006 = as.character(1:2)
                       , C006_name = c("Masculino","Feminino")) 
dic_raca <- data.table(C009 = as.character(c(1:5,9)),
                       C009_name = c("Branca","Preta","Amarela"
                                     ,"Parda","Indígena","Ignorado"))

pns19[,.N,by = .(P040,P042)]

pns19[!is.na(P040) & !is.na(P042),.N,by = .(P040,P042)]
pns19[,P04301 := as.numeric(P04301)]
pns19[,P04302 := as.numeric(P04302)]
pns19[, actv_habttime := P04301 * 60 + P04302] # Active commute time


mydiv <- function(n,d,round=3){round(100 * n/d,round)}
# sexo | raca_group | vehicleOwnership | edugroup_large | quintileBR | region
#| dummyMetro | agegroup
#|||| sexo raca-----
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18,
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               total      = sum(ww,na.rm = TRUE)
               # trab
               trab_todo       = sum(ww[P040 == "Sim, todo o trajeto"],na.rm = TRUE)
               prop_ativ       = mydiv(trab_todo,total,1)
               trab_part       = sum(ww[P040 == "Sim, parte do trajeto"],na.rm = TRUE)
               prop_trab_part  = mydiv(trab_part,trab_todo + trab_part,1)
               total_trab_ativ = sum(ww[v1410 == "Sim"],na.rm = TRUE)
               prop_trab_1d    = mydiv(n = sum(ww[P04001 == 1],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_2d    = mydiv(n = sum(ww[P04001 == 2],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_3d    = mydiv(n = sum(ww[P04001 == 3],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_4d    = mydiv(n = sum(ww[P04001 == 4],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_5d    = mydiv(n = sum(ww[P04001 == 5],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_6d    = mydiv(n = sum(ww[P04001 == 6],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_7d    = mydiv(n = sum(ww[P04001 == 7],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               trab_mean_days  = mean(P04001[v1410 == "Sim" & P04001 > 0 ],na.rm = TRUE)
               trab_mean_time  = mean(actv_commutetime[v1410 == "Sim"],na.rm = TRUE) / 2
               
               # habitual
               habt_todo      = sum(ww[P042 > 0 ],na.rm = TRUE)
               prop_habt_1d   = mydiv(n =  sum(ww[P042 == 1],na.rm = TRUE),d = habt_todo,1)
               prop_habt_2d   = mydiv(n =  sum(ww[P042 == 2],na.rm = TRUE),d = habt_todo,1)
               prop_habt_3d   = mydiv(n =  sum(ww[P042 == 3],na.rm = TRUE),d = habt_todo,1)
               prop_habt_4d   = mydiv(n =  sum(ww[P042 == 4],na.rm = TRUE),d = habt_todo,1)
               prop_habt_5d   = mydiv(n =  sum(ww[P042 == 5],na.rm = TRUE),d = habt_todo,1)
               prop_habt_6d   = mydiv(n =  sum(ww[P042 == 6],na.rm = TRUE),d = habt_todo,1)
               prop_habt_7d   = mydiv(n =  sum(ww[P042 == 7],na.rm = TRUE),d = habt_todo,1)
               habt_trab_todo = sum(ww[P042 > 0 & v1410 == "Sim"],na.rm = TRUE)
               
               habt_mean_days = mean(P042[P042 > 0 ],na.rm = TRUE)
               habt_mean_time = mean(actv_habttime[P042 > 0 ],na.rm = TRUE) / 2
               
               # return
               list(total,trab_todo      
                    , trab_part,prop_ativ, prop_trab_part, total_trab_ativ, trab_mean_days, trab_mean_time
                    , habt_todo, habt_trab_todo, habt_mean_days, habt_mean_time
                    #, prop_trab_1d, prop_trab_2d, prop_trab_3d, prop_trab_4d, prop_trab_5d, prop_trab_6d, prop_trab_7d  
                    #, prop_habt_1d, prop_habt_2d, prop_habt_3d, prop_habt_4d, prop_habt_5d, prop_habt_6d, prop_habt_7d  
               )
             },by = .(sexo,raca_group  )] %>% 
  .[raca_group %in% c("Branca","Negra"),] %>%
  #.[order(edugroup_large)] %>% .[!is.na(edugroup_large),] %>% 
  #.[!is.na(edugroup),] %>% .[c(1,3,2,4),] %>% 
  #.[!is.na(agegroup),]  %>% .[c(5,8,3,11,10,7,12,1,13),] %>% 
  .[!is.na(sexo ),] %>% .[]
#.[!is.na(edugroup ),] %>%   .[c(1,5,6,7,2,4,3,8)]

tmp1 <- tmp %>% 
  melt.data.table(., measure.vars = c("prop_ativ","prop_trab_part")
                  ,id.vars = c("sexo","raca_group")) %>% 
  .[,xlab := paste0(sexo,"-\n",raca_group)] %>% 
  .[,variable_f := factor(variable
                          ,levels = c("prop_trab_part","prop_ativ")
                          ,labels = c("Parcial","Completo"))] %>% 
  .[order(variable_f),]

tmp1[]
#sum_tmp <- tmp1[,sum(value),by = .(xlab)]  
sum_tmp <- tmp1[,{
  total <- sum(value)
  v2 <- cumsum(value)
  v3 <- total - v2
  v4 <- v3 + value/2
  list(total,"total_label" = paste0(total,"%"),"y" = v4,
       "label" = value)
},by = .(xlab)]

sum_tmp[]
ggplot(tmp1) +
  geom_col(aes(x= xlab ,y = value,fill = variable_f)
           ,width = 0.5,position = "stack")+
   #geom_point(aes(x = sexo, y = value
  #                , color = variable_f, shape = raca_group), size = 5)+
  #geom_point(aes(x = variable_f, y = value, color = xlab), size = 5)+
  geom_text(data = sum_tmp
            ,aes(x = xlab,y = y,label = label,fontface = 2)
            ,vjust = -0.25,hjust = 0.5,color = "grey90"
            ,size = 3.5)+
  geom_text(data = sum_tmp[,.SD[1],by = .(xlab)]
            ,aes(x = xlab,y = total, label = total_label,fontface = 2)
            ,vjust = -0.25,hjust = 0.5, size = 3.15)+
  labs(y = "Percentual de deslocamento \n por modos ativos (%)"
       ,x = NULL,fill = "Tipo de\ntrajeto")+
  scale_color_brewer(palette = "Set1") +
  theme_bw()

,# |||| sexo escolaridade ------
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18,
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               total      = sum(ww,na.rm = TRUE)
               # trab
               trab_todo       = sum(ww[P040 == "Sim, todo o trajeto"],na.rm = TRUE)
               prop_ativ       = mydiv(trab_todo,total,1)
               trab_part       = sum(ww[P040 == "Sim, parte do trajeto"],na.rm = TRUE)
               prop_trab_part  = mydiv(trab_part,trab_todo + trab_part,1)
               total_trab_ativ = sum(ww[v1410 == "Sim"],na.rm = TRUE)
               prop_trab_1d    = mydiv(n = sum(ww[P04001 == 1],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_2d    = mydiv(n = sum(ww[P04001 == 2],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_3d    = mydiv(n = sum(ww[P04001 == 3],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_4d    = mydiv(n = sum(ww[P04001 == 4],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_5d    = mydiv(n = sum(ww[P04001 == 5],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_6d    = mydiv(n = sum(ww[P04001 == 6],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_7d    = mydiv(n = sum(ww[P04001 == 7],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               trab_mean_days  = mean(P04001[v1410 == "Sim" & P04001 > 0 ],na.rm = TRUE)
               trab_mean_time  = mean(actv_commutetime[v1410 == "Sim"],na.rm = TRUE) / 2
               
               # habitual
               habt_todo      = sum(ww[P042 > 0 ],na.rm = TRUE)
               prop_habt_1d   = mydiv(n =  sum(ww[P042 == 1],na.rm = TRUE),d = habt_todo,1)
               prop_habt_2d   = mydiv(n =  sum(ww[P042 == 2],na.rm = TRUE),d = habt_todo,1)
               prop_habt_3d   = mydiv(n =  sum(ww[P042 == 3],na.rm = TRUE),d = habt_todo,1)
               prop_habt_4d   = mydiv(n =  sum(ww[P042 == 4],na.rm = TRUE),d = habt_todo,1)
               prop_habt_5d   = mydiv(n =  sum(ww[P042 == 5],na.rm = TRUE),d = habt_todo,1)
               prop_habt_6d   = mydiv(n =  sum(ww[P042 == 6],na.rm = TRUE),d = habt_todo,1)
               prop_habt_7d   = mydiv(n =  sum(ww[P042 == 7],na.rm = TRUE),d = habt_todo,1)
               habt_trab_todo = sum(ww[P042 > 0 & v1410 == "Sim"],na.rm = TRUE)
               
               habt_mean_days = mean(P042[P042 > 0 ],na.rm = TRUE)
               habt_mean_time = mean(actv_habttime[P042 > 0 ],na.rm = TRUE) / 2
               
               # return
               list(total,trab_todo      
                    , trab_part,prop_ativ, prop_trab_part, total_trab_ativ, trab_mean_days, trab_mean_time
                    , habt_todo, habt_trab_todo, habt_mean_days, habt_mean_time
                    #, prop_trab_1d, prop_trab_2d, prop_trab_3d, prop_trab_4d, prop_trab_5d, prop_trab_6d, prop_trab_7d  
                    #, prop_habt_1d, prop_habt_2d, prop_habt_3d, prop_habt_4d, prop_habt_5d, prop_habt_6d, prop_habt_7d  
               )
             },by = .(sexo,edugroup  )] %>% 
  #.[raca_group %in% c("Branca","Negra"),] %>%
  .[order(edugroup)] %>% .[!is.na(edugroup),] %>% 
  .[!is.na(edugroup),] %>% 
  #.[!is.na(agegroup),]  %>% .[c(5,8,3,11,10,7,12,1,13),] %>% 
  .[!is.na(sexo ),] %>% .[]
#.[!is.na(edugroup ),] %>%   .[c(1,5,6,7,2,4,3,8)]

tmp[]

tmp1 <- tmp %>% 
  melt.data.table(., measure.vars = c("prop_ativ","prop_trab_part")
                  ,id.vars = c("sexo","edugroup")) %>% 
  .[,variable_f := factor(variable,levels = c("prop_ativ","prop_trab_part")
                          ,labels = c("Completo","Parcial"))] %>% 
  .[,sexo_f := factor(sexo,levels = c("Feminino","Masculino")
                      ,labels = c("Feminino","Masculino"))] %>% 
  .[,edu_f := factor(edugroup,levels = 
                       c("Sem instrução + Fundamental incompleto",
                         "Fundamental completo",
                         "Médio completo",
                         "Superior completo"), labels = 
                       c("Sem instrução","Fundamental"
                         ,"Médio","Superior"))] %>% 
  .[order(edu_f),]

tmp1[]  

ggplot(tmp1) +
  geom_point(aes(x = edu_f, y = value,color = variable_f), size = 3)+
  geom_path(aes(x = edu_f, y = value,color = variable_f,group = variable_f), size = 1)+
  labs(y = "Percentual de deslocamento \n por modos ativos (%)"
       ,x = NULL,color = "Tipo de\ntrajeto")+
  scale_color_brewer(palette = "Set1") +
  theme_bw()+
  facet_wrap(~sexo)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))

# ||||  age ------
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18,
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               total      = sum(ww,na.rm = TRUE)
               # trab
               trab_todo       = sum(ww[P040 == "Sim, todo o trajeto"],na.rm = TRUE)
               prop_ativ       = mydiv(trab_todo,total,1)
               trab_part       = sum(ww[P040 == "Sim, parte do trajeto"],na.rm = TRUE)
               prop_trab_part  = mydiv(trab_part,trab_todo + trab_part,1)
               total_trab_ativ = sum(ww[v1410 == "Sim"],na.rm = TRUE)
               prop_trab_1d    = mydiv(n = sum(ww[P04001 == 1],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_2d    = mydiv(n = sum(ww[P04001 == 2],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_3d    = mydiv(n = sum(ww[P04001 == 3],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_4d    = mydiv(n = sum(ww[P04001 == 4],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_5d    = mydiv(n = sum(ww[P04001 == 5],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_6d    = mydiv(n = sum(ww[P04001 == 6],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_7d    = mydiv(n = sum(ww[P04001 == 7],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               trab_mean_days  = mean(P04001[v1410 == "Sim" & P04001 > 0 ],na.rm = TRUE)
               trab_mean_time  = mean(actv_commutetime[v1410 == "Sim"],na.rm = TRUE) / 2
               
               # habitual
               habt_todo      = sum(ww[P042 > 0 ],na.rm = TRUE)
               prop_habt_1d   = mydiv(n =  sum(ww[P042 == 1],na.rm = TRUE),d = habt_todo,1)
               prop_habt_2d   = mydiv(n =  sum(ww[P042 == 2],na.rm = TRUE),d = habt_todo,1)
               prop_habt_3d   = mydiv(n =  sum(ww[P042 == 3],na.rm = TRUE),d = habt_todo,1)
               prop_habt_4d   = mydiv(n =  sum(ww[P042 == 4],na.rm = TRUE),d = habt_todo,1)
               prop_habt_5d   = mydiv(n =  sum(ww[P042 == 5],na.rm = TRUE),d = habt_todo,1)
               prop_habt_6d   = mydiv(n =  sum(ww[P042 == 6],na.rm = TRUE),d = habt_todo,1)
               prop_habt_7d   = mydiv(n =  sum(ww[P042 == 7],na.rm = TRUE),d = habt_todo,1)
               habt_trab_todo = sum(ww[P042 > 0 & v1410 == "Sim"],na.rm = TRUE)
               
               habt_mean_days = mean(P042[P042 > 0 ],na.rm = TRUE)
               habt_mean_time = mean(actv_habttime[P042 > 0 ],na.rm = TRUE) / 2
               
               # return
               list(total,trab_todo      
                    , trab_part,prop_ativ, prop_trab_part, total_trab_ativ, trab_mean_days, trab_mean_time
                    , habt_todo, habt_trab_todo, habt_mean_days, habt_mean_time
                    #, prop_trab_1d, prop_trab_2d, prop_trab_3d, prop_trab_4d, prop_trab_5d, prop_trab_6d, prop_trab_7d  
                    #, prop_habt_1d, prop_habt_2d, prop_habt_3d, prop_habt_4d, prop_habt_5d, prop_habt_6d, prop_habt_7d  
               )
             },by = .(AGE)] %>% 
  .[!is.na(AGE),]

tmp[]

tmp1 <- tmp %>% 
  melt.data.table(., measure.vars = c("prop_ativ","prop_trab_part")
                  ,id.vars = c("AGE")) %>% 
  .[,variable_f := factor(variable,levels = c("prop_ativ","prop_trab_part")
                          ,labels = c("Completo","Parcial"))] %>% 
  .[,AGE_f := factor(AGE,levels = 
                       c( "18-24", "25-34", "35-44", "45-54","55-64", "65+"))] %>% 
  .[order(AGE_f),]


tmp2 <- tmp1[,{
  total <- sum(value) 
  v2 <- cumsum(value)
  v3 <- total - v2
  v4 <- v3 + value/2
  list(variable_f
       ,total,"total_label" = paste(total,"%")
       ,"y" = v4,"label" = value)
},by = .(AGE_f)] 

tmp1[]
tmp2[]

ggplot(tmp1) +
  #geom_point(aes(x = quintileBR, y = value,color = variable_f), size = 3)+
  geom_col(aes(x = AGE_f, y = value,fill = variable_f)
           ,position = "stack",width = 0.75)+
  geom_text(data = tmp2
            ,aes(x = AGE_f,y = y,label = label,fontface=2)
            ,vjust =-0.25,hjust = 0.5,size= 3.15,color = "grey90")+
  geom_text(data = tmp2[,.SD[1],by = .(AGE_f)]
            ,aes(x = AGE_f,y = total, label = total_label,fontface = 2)
            ,vjust = -0.25,hjust = 0.5, size = 3.15)+
  labs(y = "Percentual de deslocamento \n por modos ativos (%)"
       ,x = "Faixa etária",fill = "Tipo de\ntrajeto")+
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

# ||||  quintil ------
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18,
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               total      = sum(ww,na.rm = TRUE)
               # trab
               trab_todo       = sum(ww[P040 == "Sim, todo o trajeto"],na.rm = TRUE)
               prop_ativ       = mydiv(trab_todo,total,1)
               trab_part       = sum(ww[P040 == "Sim, parte do trajeto"],na.rm = TRUE)
               prop_trab_part  = mydiv(trab_part,trab_todo + trab_part,1)
               total_trab_ativ = sum(ww[v1410 == "Sim"],na.rm = TRUE)
               prop_trab_1d    = mydiv(n = sum(ww[P04001 == 1],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_2d    = mydiv(n = sum(ww[P04001 == 2],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_3d    = mydiv(n = sum(ww[P04001 == 3],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_4d    = mydiv(n = sum(ww[P04001 == 4],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_5d    = mydiv(n = sum(ww[P04001 == 5],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_6d    = mydiv(n = sum(ww[P04001 == 6],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_7d    = mydiv(n = sum(ww[P04001 == 7],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               trab_mean_days  = mean(P04001[v1410 == "Sim" & P04001 > 0 ],na.rm = TRUE)
               trab_mean_time  = mean(actv_commutetime[v1410 == "Sim"],na.rm = TRUE) / 2
               
               # habitual
               habt_todo      = sum(ww[P042 > 0 ],na.rm = TRUE)
               prop_habt_1d   = mydiv(n =  sum(ww[P042 == 1],na.rm = TRUE),d = habt_todo,1)
               prop_habt_2d   = mydiv(n =  sum(ww[P042 == 2],na.rm = TRUE),d = habt_todo,1)
               prop_habt_3d   = mydiv(n =  sum(ww[P042 == 3],na.rm = TRUE),d = habt_todo,1)
               prop_habt_4d   = mydiv(n =  sum(ww[P042 == 4],na.rm = TRUE),d = habt_todo,1)
               prop_habt_5d   = mydiv(n =  sum(ww[P042 == 5],na.rm = TRUE),d = habt_todo,1)
               prop_habt_6d   = mydiv(n =  sum(ww[P042 == 6],na.rm = TRUE),d = habt_todo,1)
               prop_habt_7d   = mydiv(n =  sum(ww[P042 == 7],na.rm = TRUE),d = habt_todo,1)
               habt_trab_todo = sum(ww[P042 > 0 & v1410 == "Sim"],na.rm = TRUE)
               
               habt_mean_days = mean(P042[P042 > 0 ],na.rm = TRUE)
               habt_mean_time = mean(actv_habttime[P042 > 0 ],na.rm = TRUE) / 2
               
               # return
               list(total,trab_todo      
                    , trab_part,prop_ativ, prop_trab_part, total_trab_ativ, trab_mean_days, trab_mean_time
                    , habt_todo, habt_trab_todo, habt_mean_days, habt_mean_time
                    #, prop_trab_1d, prop_trab_2d, prop_trab_3d, prop_trab_4d, prop_trab_5d, prop_trab_6d, prop_trab_7d  
                    #, prop_habt_1d, prop_habt_2d, prop_habt_3d, prop_habt_4d, prop_habt_5d, prop_habt_6d, prop_habt_7d  
               )
             },by = .(quintileBR)] %>% 
  #.[raca_group %in% c("Branca","Negra"),] %>%
  #.[order(edugroup)] %>% .[!is.na(edugroup),] %>% 
  .[!is.na(quintileBR),] %>%  .[]
#.[!is.na(edugroup ),] %>%   .[c(1,5,6,7,2,4,3,8)]

tmp[]

tmp1 <- tmp %>% 
  melt.data.table(., measure.vars = c("prop_ativ","prop_trab_part")
                  ,id.vars = c("quintileBR")) %>% 
  .[,variable_f := factor(variable,levels = c("prop_ativ","prop_trab_part")
                          ,labels = c("Completo","Parcial"))] %>% 
  .[order(quintileBR),]

tmp2 <- tmp1[,{
  total <- sum(value) 
  v2 <- cumsum(value)
  v3 <- total - v2
  v4 <- v3 + value/2
  list(variable_f
       ,total,"total_label" = paste(total,"%")
       ,"y" = v4,"label" = value)
},by = .(quintileBR)] 

tmp1[]
tmp2[]

ggplot(tmp1) +
  #geom_point(aes(x = quintileBR, y = value,color = variable_f), size = 3)+
  geom_col(aes(x = quintileBR, y = value,fill = variable_f)
            ,position = "stack",width = 0.75)+
  geom_text(data = tmp2
            ,aes(x = quintileBR,y = y,label = label,fontface=2)
            ,vjust =-0.25,hjust = 0.5,size= 3.15,color = "grey90")+
  geom_text(data = tmp2[,.SD[1],by = .(quintileBR)]
            ,aes(x = quintileBR,y = total, label = total_label,fontface = 2)
            ,vjust = -0.25,hjust = 0.5, size = 3.15)+
  labs(y = "Percentual de deslocamento \n por modos ativos (%)"
       ,x = "Quintil de renda",fill = "Tipo de\ntrajeto")+
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

# ||||  edugroup ------
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18,
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               total      = sum(ww,na.rm = TRUE)
               # trab
               trab_todo       = sum(ww[P040 == "Sim, todo o trajeto"],na.rm = TRUE)
               prop_ativ       = mydiv(trab_todo,total,1)
               trab_part       = sum(ww[P040 == "Sim, parte do trajeto"],na.rm = TRUE)
               prop_trab_part  = mydiv(trab_part,trab_todo + trab_part,1)
               total_trab_ativ = sum(ww[v1410 == "Sim"],na.rm = TRUE)
               prop_trab_1d    = mydiv(n = sum(ww[P04001 == 1],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_2d    = mydiv(n = sum(ww[P04001 == 2],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_3d    = mydiv(n = sum(ww[P04001 == 3],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_4d    = mydiv(n = sum(ww[P04001 == 4],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_5d    = mydiv(n = sum(ww[P04001 == 5],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_6d    = mydiv(n = sum(ww[P04001 == 6],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               prop_trab_7d    = mydiv(n = sum(ww[P04001 == 7],na.rm = TRUE),d = sum(ww[P04001 > 0],na.rm = TRUE),1)
               trab_mean_days  = mean(P04001[v1410 == "Sim" & P04001 > 0 ],na.rm = TRUE)
               trab_mean_time  = mean(actv_commutetime[v1410 == "Sim"],na.rm = TRUE) / 2
               
               # habitual
               habt_todo      = sum(ww[P042 > 0 ],na.rm = TRUE)
               prop_habt_1d   = mydiv(n =  sum(ww[P042 == 1],na.rm = TRUE),d = habt_todo,1)
               prop_habt_2d   = mydiv(n =  sum(ww[P042 == 2],na.rm = TRUE),d = habt_todo,1)
               prop_habt_3d   = mydiv(n =  sum(ww[P042 == 3],na.rm = TRUE),d = habt_todo,1)
               prop_habt_4d   = mydiv(n =  sum(ww[P042 == 4],na.rm = TRUE),d = habt_todo,1)
               prop_habt_5d   = mydiv(n =  sum(ww[P042 == 5],na.rm = TRUE),d = habt_todo,1)
               prop_habt_6d   = mydiv(n =  sum(ww[P042 == 6],na.rm = TRUE),d = habt_todo,1)
               prop_habt_7d   = mydiv(n =  sum(ww[P042 == 7],na.rm = TRUE),d = habt_todo,1)
               habt_trab_todo = sum(ww[P042 > 0 & v1410 == "Sim"],na.rm = TRUE)
               
               habt_mean_days = mean(P042[P042 > 0 ],na.rm = TRUE)
               habt_mean_time = mean(actv_habttime[P042 > 0 ],na.rm = TRUE) / 2
               
               # return
               list(total,trab_todo      
                    , trab_part,prop_ativ, prop_trab_part, total_trab_ativ, trab_mean_days, trab_mean_time
                    , habt_todo, habt_trab_todo, habt_mean_days, habt_mean_time
                    #, prop_trab_1d, prop_trab_2d, prop_trab_3d, prop_trab_4d, prop_trab_5d, prop_trab_6d, prop_trab_7d  
                    #, prop_habt_1d, prop_habt_2d, prop_habt_3d, prop_habt_4d, prop_habt_5d, prop_habt_6d, prop_habt_7d  
               )
             },by = .(edugroup)] %>% 
  #.[raca_group %in% c("Branca","Negra"),] %>%
  #.[order(edugroup)] %>% .[!is.na(edugroup),] %>% 
  .[!is.na(edugroup),] %>%  .[]
#.[!is.na(edugroup ),] %>%   .[c(1,5,6,7,2,4,3,8)]

tmp[]

tmp1 <- tmp %>% 
  melt.data.table(., measure.vars = c("prop_ativ","prop_trab_part")
                  ,id.vars = c("edugroup")) %>% 
  .[,variable_f := factor(variable,levels = c("prop_ativ","prop_trab_part")
                          ,labels = c("Completo","Parcial"))] %>% 
  .[,edu_f := factor(edugroup,levels = 
                       c("Sem instrução + Fundamental incompleto",
                         "Fundamental completo",
                         "Médio completo",
                         "Superior completo"), labels = 
                       c("Sem instrução","Fundamental"
                         ,"Médio","Superior"))] %>% 
  .[order(edu_f),]

tmp1[]

tmp2 <- tmp1[,{
  total <- sum(value) 
  v2 <- cumsum(value)
  v3 <- total - v2
  v4 <- v3 + value/2
  list(variable_f
       ,total,"total_label" = paste(total,"%")
       ,"y" = v4,"label" = value)
},by = .(edu_f)] 

tmp1[]
tmp2[]
ggplot(tmp1) +
  #geom_point(aes(x = quintileBR, y = value,color = variable_f), size = 3)+
  geom_col(aes(x = edu_f, y = value,fill = variable_f)
           ,position = "stack",width = 0.75)+
  geom_text(data = tmp2
            ,aes(x = edu_f,y = y,label = label,fontface = 2)
            ,vjust = -0.25,hjust = 0.5,color = "grey90"
            ,size = 3.5)+
  geom_text(data = tmp2[,.SD[1],by = .(edu_f)]
            ,aes(x = edu_f,y = total, label = total_label,fontface = 2)
            ,vjust = -0.25,hjust = 0.5, size = 3.15)+
  labs(y = "Percentual de deslocamento \n por modos ativos (%)"
       ,x = "Escolaridade",fill = "Tipo de\ntrajeto")+
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

# 4) Entre as pessoas que fazem
# atividades habituais e tem info de renda
# , qual é a média de numero de dias que 
# se deslocam a pé ou bici

plot4 <- pns19[P042 > 0 & !is.na(P042) & !is.na(quintileBR)
               ,.N
               ,by = .(P042,C006 ,quintileBR)]
plot4 <- plot4[, matrixStats::weightedMedian(x = P042, w = N)
               ,by = .(quintileBR,C006)]
plot4 <- plot4[dic_sexo,on = "C006"]

p2 <- ggplot(plot4)+
  geom_point(aes(y = V1,x = as.factor(quintileBR)
                 , color = C006_name),size = 7)+
  scale_y_continuous(limits = c(3,5))+
  labs(y = "Numero medio de dias"
       , x = "quintil de renda"
       , fill = "sexo"
       , title = "Numero médiano de dias \n de atividades habituais"
       , caption = "*para pessoas que fazem atv. hab. pelo menos 1x semana")

p2
# trab
plot41 <- pns19[P04001 > 0 & !is.na(P04001) & !is.na(quintileBR)
                ,.N
                ,by = .(P04001,C006 ,quintileBR)]
plot41 <- plot41[, matrixStats::weightedMedian(x = P04001, w = N)
                 ,by = .(quintileBR,C006)]
plot41 <- plot41[dic_sexo,on = "C006"]
plot41
p1 <- ggplot(plot41)+
  geom_point(aes(y = V1,x = as.factor(quintileBR)
                 , color = C006_name),size = 7)+
  scale_y_continuous(limits = c(3,6))+
  labs(y = "Numero medio de dias"
       , x = "quintil de renda"
       , fill = "sexo"
       , title = "Numero médio de dias \n de atividades TRABALHO"
       , caption = "*para pessoas que fazem atv. hab. pelo menos 1x semana")

p1
p1 | p2
# 5) Entre quem se desloca a pé/bici, qual a proporcao de dias
# de uso que sao utilizados para atividades habituais?

plot5 <- pns19[!is.na(P042) & !is.na(P04001) & 
                 !is.na(quintileBR)
               ,.N
               ,by = .(P042,P04001,C006,quintileBR)]
plot5[,total_dias := P042 + P04001]
plot5 <- plot5[total_dias > 0,]
plot5 <- plot5[,weighted.mean(
  x = P042 / total_dias
  ,w = N), by = .(C006,quintileBR)]

plot5 <- plot5[dic_sexo,on = "C006"]

ggplot(plot5)+
  geom_point(aes(x = V1,y = as.factor(quintileBR)
                 , color = C006_name),size = 7)+
  scale_x_continuous(labels = scales::percent)+
  labs(x = "Proporcao (%)"
       , y = "quintil de renda"
       , fill = "sexo"
       , title = "Proporcao do numero de dias de deslocamento a pé/bici\n para fins de atividades habituais"
       , caption = "*para pessoas que deslocam a pé ou por bici pelo menos um vez por semana\n
       proporcao = ndias_ativ_hab / (ndias_ativ_hab + ndias_trabalho) ")

# adiciona legenda


pns2019cp <- pns2019cp[dic_sexo, on = "C006"]
pns2019cp <- pns2019cp[dic_raca, on = "C009"]


# razao_dias = dias atividades habituais / dias total
# razao_dias = P042 / P04001

nrow( pns2019cp )                 # 18717
nrow( pns2019cp[P042 > P04001,] ) # 1940 (~10 %)

pns2019cp[, razao_dias := P042 / (P04001 + P042)]
pns2019cp[,mean(razao_dias,na.rm = TRUE), by = C006_name]
pns2019cp[,mean(razao_dias,na.rm = TRUE), by = .(C009_name,C006_name)]

# media do numero de dias (atividades habituais)
pns2019cp[,summary(P042,na.rm = TRUE), by = C006_name]

# media do numero de dias (atividades totais)
pns2019cp[,mean(P04001,na.rm = TRUE), by = C006_name]

# proporcao de dias por genero
prop_dias_P042 <- pns2019cp[,.N,by = .(C006_name,P042)]
prop_dias_P042[,prop := round(100 * N/sum(N), 2),by = .(C006_name)]
prop_dias_P042[,dias_f := factor(x = P042,levels = 0:7)]
prop_dias_P042[,name := "ativ_habituais"]
prop_dias_P042[,P042 := NULL]

prop_dias_P04001 <- pns2019cp[,.N,by = .(C006_name,P04001)]
prop_dias_P04001[,prop := round(100 * N/sum(N), 2),by = .(C006_name)]
prop_dias_P04001[,dias_f := factor(x = P04001,levels = 0:7)]
prop_dias_P04001[,name := "trabalho"]
prop_dias_P04001[,P04001 := NULL]

prop_dias <- rbind(prop_dias_P04001,prop_dias_P042)
prop_dias[,name_f := factor(name,c("total","ativ_habituais"))]

ggplot(prop_dias)+
  geom_bar(aes(x = dias_f, y = prop, fill = factor(C006_name))
           ,stat = "identity",position = "dodge2")+
  facet_wrap(~name_f,ncol = 1,scales = "free_y")+
  labs(x = "numero de dias da semana")

pns2019cp$P04001 %>% summary()
pns2019[
  as.numeric(V0026) == 1 #&   # urbano
  P040 == "1"                # 1  Sim, todo o trajeto
  ,.N
  ,by = .(VDD004A)]            # Nivel de instrucao



# 5) EXP acidents ----
rm(list=ls()) 
data_path <- "../../data/transporte_ativo_2008-2019/" 
data_path <- "data/"
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(patchwork)

pns19 <- readr::read_rds(paste0(data_path,"pns2019_dt.rds"))
#pns19 <- pns19[]

# O009	O9	 Nos últimos doze meses, o(a) Sr(a) se envolveu em algum acidente de 
# trânsito no qual tenha sofrido lesões corporais (ferimentos)
# 1	Sim
# 2	Não
# Não aplicável
pns19[V0025A == "1",sum(V00291),by = "O009"]
pns19[,sum(V00291),by = "O009"]


# O10	Algum desses acidentes de trânsito ocorreu quando o(a) Sr(a) estava 
# trabalhando, indo ou voltando do trabalho
# 1	Sim, quando estava trabalhando
# 2	Sim, quando estava indo ou voltando do trabalho 
# 3	Não
# Não aplicável
pns19[,O010_label := data.table::fcase(
  O010 == 1, "Sim, quando estava trabalhando"
  , O010 == 2, "Sim, quando estava indo ou voltando do trabalho"
  , O010 == 3, "Não")]

pns19[V0025A == "1" & O009 == "1",sum(V00291),by = "O010"]

# O01102	O11a	Durante o acidente de trânsito ocorrido nos últimos 12 meses,
# o(a) Sr(a) era: (Se houver mais de um, considere o mais grave)
# 01	 Condutor(a) de automóvel (inclusive táxi, aplicativos de transporte e similares)
# 02 	Condutor(a) de ônibus 
# 03	 Condutor (a) de caminhão
# 04	 Condutor(a) de motocicleta
# 05	 Condutor(a) de bicicleta
# 06	 Passageiro(a) de automóvel (inclusive táxi, aplicativos de transporte e similares) 
# 07	 Passageiro(a) de ônibus 
# 08	Passageiro (a) de caminhão
# 09	 Passageiro(a) de motocicleta
# 10	 Passageiro(a) de bicicleta  
# 11	 Pedestre
# 12	Outro
# Não aplicável

pns19[,O01102_label := data.table::fcase(
  O01102 ==  1, "Condutor(a) de carro/van",
  O01102 ==  2, "Condutor(a) de ônibus",
  O01102 ==  3, "Condutor(a) de caminhão",
  O01102 ==  4, "Condutor(a) de motocicleta",
  O01102 ==  5, "Condutor(a) de bicicleta",
  O01102 ==  6, "Passageiro(a) de carro/van",
  O01102 ==  7, "Passageiro(a) de ônibus",
  O01102 ==  8, "Passageiro(a) de caminhão",
  O01102 ==  9, "Passageiro(a) de motocicleta",
  O01102 == 10, "Passageiro(a) de bicicleta",
  O01102 == 11, "Pedestre",
  O01102 == 11, "Outro",
  O01102 == 11, "Não aplicável")]
pns19[O009 == "1",sum(V00291),by = c("O010","O01102_label")]
pns19[O009 == "1" & O010 == 2,sum(V00291),by = c("O01102_label")]

pns19[,P04001_condition := fifelse(P04001 > 0,"Pelo menos um dia","Nenhum")]
pns19[,P042_condition := fifelse(P042 > 0,"Pelo menos um dia","Nenhum")]
# - total pessoas que se acidentaram conforme
# deslocamento a pé/bicicleta para fins habituais 

pns19[V0025A == "1" & O009 == "1" & O01102 %in% c(5,10,11)
      ,list(sum(V00291),.N)
      ,by = .(P042_condition)]

# - total pessoas que se acidentaram conforme
# deslocamento a pé/bicicleta para fins de trabalho
# (filtro ativo)
pns19[O009 == "1" & O01102 %in% c(5,10,11)
      ,list(sum(V00291),.N)
      ,by = .(P040)] 

# - total pessoas que se acidentaram conforme
# deslocamento a pé/bicicleta para fins de trabalho e habituais
# (filtro ativo)

pns19[V0025A == "1" & O009 == "1" & O01102 %in% c(5,10,11)
      ,list(sum(V00291),.N)
      ,by = .(P040,P042_condition)]

# - total pessoas que se acidentam conforme
# condicao do acidente e deslocamento
# a pé/bicicleta para fins de trabalho (filtro ativo)
pns19[V0025A == "1" & O01102 %in% c(5,10,11)
      ,list("soma_morador" = sum(V00291),"N" = .N)
      ,by = .(v1410 ,O01102_label)]

# total de pessoas que se acidentam e responderam
# sobre deslocamento a pé/bici, conforme
# condicao do acidente
pns19[V0025A == "1" & !is.na(O01102) & !is.na(P040)
      ,list("soma_morador" = sum(V00291),"N" = .N)
      ,by = .(v1410 ,O01102_label)] 

# total de pessoas que se acidentam e responderam
# sobre deslocamento a pé/bici, conforme
# condicao do acidente (filtro modo ativo)
pns19[V0025A == "1" & !is.na(O01102) & !is.na(P040) &
        O01102 %in% c(5,10,11)
      ,list(sum(V00291),.N)
      ,by = .(v1410 ,O01102_label)] 

#|||| RACA acidente ---- 

pns19[,O01102_activ := fifelse(O01102 %in% c(5,10,11),"Active","Motorized")]


# proporcao racial que faz deslocamento ativo p/trabalho

mydiv <- function(n,d,round=3){round(100 * n/d,round)}
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18 & 
               !is.na(raca_group),
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               
               total      = sum(ww,na.rm = TRUE)
               total_ativ = sum(ww[V0025A == "1" & v1410 == "Sim"],na.rm = TRUE)
               prop_ativ  = mydiv(n = total_ativ,d = total,round = 1)
               
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_bici_acid = sum(ww[V0025A == "1" & 
                                          v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10)],na.rm = TRUE) 
               prop_bici_acid  = mydiv(n = total_bici_acid,d = total_ativ,round = 2)
               
               total_walk_acid = sum(ww[V0025A == "1" & 
                                          v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(11)],na.rm = TRUE) 
               prop_walk_acid  = mydiv(n = total_walk_acid,d = total_ativ,round = 2)
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5,10,11)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_ativ_acid = sum(ww[V0025A == "1" & 
                                          v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10,11)],na.rm = TRUE) 
               prop_ativ_acid = mydiv(n = total_ativ_acid,d = total_ativ,round = 2)
               
               # return
               list(total,total_ativ,prop_ativ,
                    total_bici_acid,prop_bici_acid,  
                    total_walk_acid,prop_walk_acid,
                    total_ativ_acid,prop_ativ_acid)
             },by = .(raca_group)]

tmp %>% .[raca_group %in% c("Branca","Negra"),]

# ||| SEXO acidente ---- 

# proporcao sexo populacao
mydiv <- function(n,d,round=3){round(100 * n/d,round)}

tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18 & 
               !is.na(sexo),
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               total      = sum(ww,na.rm = TRUE)
               total_ativ = sum(ww[v1410 == "Sim"],na.rm = TRUE)
               prop_ativ  = mydiv(n = total_ativ,d = total,round = 1)
               
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_bici_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10)],na.rm = TRUE) 
               prop_bici_acid  = mydiv(n = total_bici_acid,d = total_ativ,round = 2)
               
               total_walk_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(11)],na.rm = TRUE) 
               prop_walk_acid  = mydiv(n = total_walk_acid,d = total_ativ,round = 2)
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5,10,11)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_ativ_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10,11)],na.rm = TRUE) 
               prop_ativ_acid = mydiv(n = total_ativ_acid,d = total_ativ,round = 2)
               
               # return
               list(total,total_ativ,prop_ativ,
                    total_bici_acid,prop_bici_acid,  
                    total_walk_acid,prop_walk_acid,
                    total_ativ_acid,prop_ativ_acid)
             },by = .(sexo)]
tmp[]

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "sexo")
openxlsx::writeDataTable(wb,sheet = "sexo",x = tmp)
saveWorkbook(wb, "data/pns19_output.xlsx", overwrite = TRUE)


# ||| AGE acidente ---- 
# proporcao sexo populacao
mydiv <- function(n,d,r = 2){round(n/(d / 100000),r)}
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18 & 
               !is.na(raca_group),
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               Num <- .N
               
               # add diference on denominator
               ids_ativ   = which(v1410 == "Sim")
               ids_acid_total = which(V0025A == "1" & v1410 == "Sim" & 
                                        `O010` == 2 & !is.na(O01102))
               
               sub <- setdiff(ids_acid_total,ids_ativ)
               if(length(sub) > 0) ids_ativ <- c(ids_ativ,sub)

               # stats
               total      = sum(ww,na.rm = TRUE)
               total_ativ = sum(ww[ids_ativ],na.rm = TRUE)
               N_ativ         = length(Num[v1410 == "Sim"])
               N_acid_total   = length(Num[V0025A == "1" & v1410 == "Sim" & 
                                             `O010` == 2 & !is.na(O01102)])
               N_acid_ativ    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(5,10,11)])
               N_acid_walk    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(11)])
               N_acid_bici    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(5,10)])
               
               prop_ativ  = mydiv(n = total_ativ,d = total)
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_bici_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10)],na.rm = TRUE) 
               prop_bici_acid  = mydiv(n = total_bici_acid,d = total_ativ)
               
               total_walk_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(11)],na.rm = TRUE) 
               prop_walk_acid  = mydiv(n = total_walk_acid,d = total_ativ)
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5,10,11)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_ativ_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10,11)],na.rm = TRUE) 
               prop_ativ_acid = mydiv(n = total_ativ_acid,d = total_ativ)
               
               # return
               list(total,N_ativ,N_acid_total,N_acid_ativ,N_acid_walk,N_acid_bici,
                    total_ativ,prop_ativ,
                    total_bici_acid,prop_bici_acid,  
                    total_walk_acid,prop_walk_acid,
                    total_ativ_acid,prop_ativ_acid)
             },by = .(AGE)]

tmp[]

tmp1 <- tmp %>% 
 # melt.data.table(., measure.vars = c("N_acid_walk", "N_acid_bici")
  melt.data.table(., measure.vars = c("prop_walk_acid", "prop_bici_acid")
                  ,id.vars = c("AGE")) %>% 
  #.[,variable_f := factor(variable,levels = c("N_acid_walk","N_acid_bici")
  .[,variable_f := factor(variable,levels = c("prop_walk_acid", "prop_bici_acid")
                          ,labels = c("A pé","Bicicleta"))] %>% 
  .[,AGE_f := factor(AGE,levels = 
                       c( "18-24", "25-34", "35-44", "45-54","55-64", "65+"))] %>% 
  .[order(AGE_f),]

tmp1[]

tmp2 <- tmp1[,{
  total <- sum(value) 
  v2 <- cumsum(value)
  v3 <- total - v2
  v4 <- v3 + value/2
  list(variable_f
       ,total,"total_label" = total
       ,"y" = v4,"label" = value)
},by = .(AGE_f)] 

tmp2[]
ggplot(tmp1) +
  #geom_point(aes(x = quintileBR, y = value,color = variable_f), size = 3)+
  geom_col(aes(x = AGE_f, y = value,fill = variable_f)
           ,position = "stack",width = 0.75)+
  geom_text(data = tmp2
            ,aes(x = AGE_f,y = y,label = label,fontface=2)
            ,vjust =-0.25,hjust = 0.5,size= 3.15)+
  labs(y = "Número de acidentes \n a cada 100 mil hab."
       ,x = "Intervalo de idade",fill = "Modo de\n deslocamento")+
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

#||| edugroup acidente ---- 
pns19[,actv_commutetime_dummy := fcase(
  actv_commutetime_00to09 == 1,"00to09",
  actv_commutetime_10to19 == 1,"10to19",
  actv_commutetime_20to29 == 1,"20to29",
  actv_commutetime_30to44 == 1,"30to44",
  actv_commutetime_45to59 == 1,"45to59",
  actv_commutetime_from60 == 1,"from60"
)]
# proporcao sexo populacao
mydiv <- function(n,d,r = 2){round(n/(d / 100000),r)}
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18 & 
               !is.na(raca_group),
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               Num <- .N
               
               # add diference on denominator
               ids_ativ   = which(v1410 == "Sim")
               ids_acid_total = which(V0025A == "1" & v1410 == "Sim" & 
                                        `O010` == 2 & !is.na(O01102))
               
               sub <- setdiff(ids_acid_total,ids_ativ)
               if(length(sub) > 0) ids_ativ <- c(ids_ativ,sub)
               
               # stats
               total      = sum(ww,na.rm = TRUE)
               total_ativ = sum(ww[ids_ativ],na.rm = TRUE)
               N_ativ         = length(Num[v1410 == "Sim"])
               N_acid_total   = length(Num[V0025A == "1" & v1410 == "Sim" & 
                                             `O010` == 2 & !is.na(O01102)])
               N_acid_ativ    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(5,10,11)])
               N_acid_walk    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(11)])
               N_acid_bici    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(5,10)])
               
               prop_ativ  = mydiv(n = total_ativ,d = total)
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_bici_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10)],na.rm = TRUE) 
               prop_bici_acid  = mydiv(n = total_bici_acid,d = total_ativ)
               
               total_walk_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(11)],na.rm = TRUE) 
               prop_walk_acid  = mydiv(n = total_walk_acid,d = total_ativ)
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5,10,11)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_ativ_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10,11)],na.rm = TRUE) 
               prop_ativ_acid = mydiv(n = total_ativ_acid,d = total_ativ)
               
               # return
               list(total,N_ativ,N_acid_total,N_acid_ativ,N_acid_walk,N_acid_bici,
                    total_ativ,prop_ativ,
                    total_bici_acid,prop_bici_acid,  
                    total_walk_acid,prop_walk_acid,
                    total_ativ_acid,prop_ativ_acid)
             },by = .(edugroup)]

tmp[]

tmp1 <- tmp %>% 
  # melt.data.table(., measure.vars = c("N_acid_walk", "N_acid_bici")
  melt.data.table(., measure.vars = c("prop_walk_acid", "prop_bici_acid")
                  ,id.vars = c("edugroup")) %>% 
  #.[,variable_f := factor(variable,levels = c("N_acid_walk","N_acid_bici")
  .[,variable_f := factor(variable,levels = c("prop_walk_acid", "prop_bici_acid")
                          ,labels = c("A pé","Bicicleta"))] %>% 
  .[,edu_f := factor(edugroup,levels = 
                       c("Sem instrução + Fundamental incompleto",
                         "Fundamental completo",
                         "Médio completo",
                         "Superior completo"), labels = 
                       c("Sem instrução","Fundamental"
                         ,"Médio","Superior"))] %>% 
  .[order(edu_f),]

tmp1[]

tmp2 <- tmp1[,{
  total <- sum(value) 
  v2 <- cumsum(value)
  v3 <- total - v2
  v4 <- v3 + value/2
  list(variable_f
       ,total,"total_label" = total
       ,"y" = v4,"label" = value)
},by = .(edu_f)] 

tmp2[]
ggplot(tmp1) +
  #geom_point(aes(x = quintileBR, y = value,color = variable_f), size = 3)+
  geom_col(aes(x = edu_f, y = value,fill = variable_f)
           ,position = "stack",width = 0.75)+
  geom_text(data = tmp2
            ,aes(x = edu_f,y = y,label = label,fontface=2)
            ,vjust =-0.25,hjust = 0.5,size= 3.15)+
  labs(y = "Número de acidentes \n a cada 100 mil hab."
       ,x = "Escolaridade",fill = "Modo de\n deslocamento")+
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

#||| renda acidente ---- 
mydiv <- function(n,d,r = 2){round(n/(d / 100000),r)}
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18 & 
               !is.na(raca_group),
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               Num <- .N
               
               # add diference on denominator
               ids_ativ   = which(v1410 == "Sim")
               ids_acid_total = which(V0025A == "1" & v1410 == "Sim" & 
                                        `O010` == 2 & !is.na(O01102))
               
               sub <- setdiff(ids_acid_total,ids_ativ)
               if(length(sub) > 0) ids_ativ <- c(ids_ativ,sub)
               
               # stats
               total      = sum(ww,na.rm = TRUE)
               total_ativ = sum(ww[ids_ativ],na.rm = TRUE)
               N_ativ         = length(Num[v1410 == "Sim"])
               N_acid_total   = length(Num[V0025A == "1" & v1410 == "Sim" & 
                                             `O010` == 2 & !is.na(O01102)])
               N_acid_ativ    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(5,10,11)])
               N_acid_walk    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(11)])
               N_acid_bici    = length(Num[V0025A == "1" & v1410 == "Sim" &
                                             `O010` == 2 & O01102 %in% c(5,10)])
               
               prop_ativ  = mydiv(n = total_ativ,d = total)
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_bici_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10)],na.rm = TRUE) 
               prop_bici_acid  = mydiv(n = total_bici_acid,d = total_ativ)
               
               total_walk_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(11)],na.rm = TRUE) 
               prop_walk_acid  = mydiv(n = total_walk_acid,d = total_ativ)
               # total pessoas que &
               # sofreram acidente de percuro de ida/volta de trabalho (`O010` == 2) &&
               # utilizando modos ativos como meio de transporte [O01102 %in% c(5,10,11)]
               # (pedrestes / ocupantes bici / passageira bici)
               total_ativ_acid = sum(ww[V0025A == "1" & v1410 == "Sim" & `O010` == 2 & 
                                          O01102 %in% c(5,10,11)],na.rm = TRUE) 
               prop_ativ_acid = mydiv(n = total_ativ_acid,d = total_ativ)
               
               # return
               list(total,N_ativ,N_acid_total,N_acid_ativ,N_acid_walk,N_acid_bici,
                    total_ativ,prop_ativ,
                    total_bici_acid,prop_bici_acid,  
                    total_walk_acid,prop_walk_acid,
                    total_ativ_acid,prop_ativ_acid)
             },by = .(quintileBR)] %>% .[!is.na(quintileBR)]

tmp[]

tmp1 <- tmp %>% 
  melt.data.table(., measure.vars = c("prop_walk_acid", "prop_bici_acid")
                  ,id.vars = c("quintileBR")) %>% 
  .[,variable_f := factor(variable,levels = c("prop_walk_acid", "prop_bici_acid")
                          ,labels = c("A pé","Bicicleta"))] %>% 
  .[,quintileBR_f := factor(quintileBR,levels = 1:5)] %>% 
  .[order(quintileBR_f),]

tmp1[]

tmp2 <- tmp1[,{
  total <- sum(value) 
  v2 <- cumsum(value)
  v3 <- total - v2
  v4 <- v3 + value/2
  list(variable_f
       ,total,"total_label" = total
       ,"y" = v4,"label" = value)
},by = .(quintileBR_f)] 

tmp2[]
ggplot(tmp1) +
  #geom_point(aes(x = quintileBR, y = value,color = variable_f), size = 3)+
  geom_col(aes(x = quintileBR_f, y = value,fill = variable_f)
           ,position = "stack",width = 0.75)+
  geom_text(data = tmp2
            ,aes(x = quintileBR_f,y = y,label = label,fontface=2)
            ,vjust =-0.25,hjust = 0.5,size= 3.15)+
  labs(y = "Número de acidentes \n a cada 100 mil hab."
       ,x = "Quintil de renda",fill = "Modo de\n deslocamento")+
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

# ||| extra proporcao mulheres -----
rm(list=ls()) data_path <- "../../data/transporte_ativo_2008-2019/" data_path <- "data/"
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(patchwork)

pns19 <- readr::read_rds("data/pns2019_dt.rds")

pns19 %>% names() %>% sort()
pns19$urban %>% unique()
pns19$sexo %>% unique()
pns19$O010 %>% unique()
pns19$O01102 %>% unique()

mydiv <- function(n,d,r = 2){round(100 * n/d,r)}
mydiv_10k <- function(n,d,r = 2){round(n/(d / 100000),r)}
tmp <- pns19[V0025A == "1" & 
               urban == "Urbano" &
               !is.na(peso_morador_selec) &
               !is.na(P040) &
               C008 >= 18,
             # inicio operation 
             {
               # define peso
               #ww <- V00291
               ww <- peso_morador_selec
               Num          = .N
               N_ativ       = length(Num[v1410 == "Sim"])
               N_ativ_women = length(Num[v1410 == "Sim" & sexo == "Feminino"])
               N_acid_total = length(Num[V0025A == "1" & v1410 == "Sim" & !is.na(O01102)])
               N_acid_ativ  = length(Num[V0025A == "1" & v1410 == "Sim" & O01102 %in% c(5,10,11)])
               
               total      = sum(ww,na.rm = TRUE)
               total_ativ = sum(ww[v1410 == "Sim"],na.rm = TRUE)
               
               total_women      = sum(ww[sexo == "Feminino"],na.rm = TRUE)
               total_ativ_women = sum(ww[v1410 == "Sim" & sexo == "Feminino"],na.rm = TRUE)
               prop_ativ_women  = mydiv(n = total_ativ_women,d = total_women,3)
               
               
               total_acid = sum(ww[V0025A == "1" & v1410 == "Sim" &  
                                     !is.na(O01102)],na.rm = TRUE) 
               total_ativ_acid = sum(ww[V0025A == "1" & v1410 == "Sim" &
                                          O01102 %in% c(5,10,11)],na.rm = TRUE) 
               # acid a cada 100 mil hab
               prop_acid = mydiv_10k(total_acid,total,2)
               prop_ativ_acid = mydiv_10k(total_ativ_acid,total,2) 
               
               list(
                 Num,N_ativ,N_ativ_women,N_acid_total,N_acid_ativ ,
                 total,total_ativ,
                 total_women,total_ativ_women,prop_ativ_women,
                 total_acid,total_ativ_acid,prop_acid,prop_ativ_acid
               )
             },by = .(metro)] %>% .[!is.na(metro),]

tmp[]

tmp %>% 
  ggplot()+
  geom_point(aes(y = prop_ativ_women,x = prop_ativ_acid,color = metro),size = 4)


tmp %>% 
  .[total_acid > 0 | total_ativ_acid > 0,] %>% 
  .[!is.na(metro),] %>% 
  #.[metro != "Restante das UF"] %>% 
  ggplot()+
  geom_point(aes(y = prop_ativ_women,x = prop_ativ_acid,color = metro),size = 4)+
  labs(x="Proporcao de acidentes de modos \nativos (ocorrencias/100 mil hab)"
       ,y= "Proporcao de mulheres que se \ndeslocam por modos ativos (%)"
       ,color = "Regiao \nMetropolitana"
       ,title = "Prop mulheres X acidentes modos ativos")

tmp %>% 
  .[total_acid > 0 | total_ativ_acid > 0,] %>% 
  .[!is.na(metro),] %>% 
  # .[metro != "Restante das UF"] %>% 
  ggplot()+
  geom_point(aes(y = prop_ativ_women,x = prop_acid,color = metro),size = 4)+
  labs(x="Proporcao de acidentes totais\n (ocorrencias/100 mil hab)"
       ,y= "Proporcao de mulheres que se \ndeslocam por modos ativos (%)"
       ,color = "Regiao \nMetropolitana"
       ,title = "Prop mulheres X acidentes totais")

# |||xxxxxx-----
rm(list=ls()) data_path <- "../../data/transporte_ativo_2008-2019/" data_path <- "data/"
gc(reset = TRUE)
library(ggplot2)
library(data.table)
library(magrittr)
library(patchwork)

pns19 <- readr::read_rds("data/pns2019_dt.rds")

pns19 %>% names() %>% sort()
pns19$urban %>% unique()
pns19$sexo %>% unique()
pns19$O010 %>% unique()
pns19$O01102 %>% unique()

mydiv <- function(n,d,r = 2){round(n/(d / 100000),r)}
tmp <- pns19[urban == "Urbano",
             {
               Num          = .N
               N_ativ       = length(Num[v1410 == "Sim"])
               
               total      = sum(V00291,na.rm = TRUE)
               total_ativ = sum(V00291[v1410 == "Sim"],na.rm = TRUE)
               prop_ativ  = mydiv(n = total_ativ,d = total,r = 1)
               
               total_men      = sum(V00291[sexo == "Masculino"],na.rm = TRUE)
               total_ativ_men = sum(V00291[v1410 == "Sim" & sexo == "Masculino"],na.rm = TRUE)
               prop_ativ_men  = mydiv(n = total_ativ_men,d = total_men,r = 1)
               
               total_women      = sum(V00291[sexo == "Feminino"],na.rm = TRUE)
               total_ativ_women = sum(V00291[v1410 == "Sim" & sexo == "Feminino"],na.rm = TRUE)
               prop_ativ_women  = mydiv(n = total_ativ_women,d = total_women,r = 1)
               
               list(
                 total,total_ativ,
                 total_men,total_ativ_men,
                 total_women,total_ativ_women,
                 prop_ativ,prop_ativ_men,prop_ativ_women
               )
             },by = .(edugroup)]

tmp[total  > 0][c(1,3,2,4)]

tmp %>% 
  ggplot()+
  geom_point(aes(y = prop_ativ_women,x = prop_ativ_acid,color = metro),size = 4)


tmp %>% 
  .[total_acid > 0 | total_ativ_acid > 0,] %>% 
  .[!is.na(metro),] %>% 
  .[metro != "Restante das UF"] %>% 
  ggplot()+
  geom_point(aes(y = prop_ativ_women,x = prop_ativ_acid,color = metro),size = 4)+
  labs(x="Proporcao de acidentes de modos \nativos (ocorrencias/100 mil hab)"
       ,y= "Proporcao de mulheres que se \ndeslocam por modos ativos (%)"
       ,color = "Regiao \nMetropolitana"
       ,title = "Prop mulheres X acidentes modos ativos")

tmp %>% 
  .[total_acid > 0 | total_ativ_acid > 0,] %>% 
  .[!is.na(metro),] %>% 
  # .[metro != "Restante das UF"] %>% 
  ggplot()+
  geom_point(aes(y = prop_ativ_women,x = prop_acid,color = metro),size = 4)+
  labs(x="Proporcao de acidentes totais\n (ocorrencias/100 mil hab)"
       ,y= "Proporcao de mulheres que se \ndeslocam por modos ativos (%)"
       ,color = "Regiao \nMetropolitana"
       ,title = "Prop mulheres X acidentes totais")

# end----