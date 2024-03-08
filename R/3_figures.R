# load and read -----
rm(list=ls()) 
data_path <- "../../data/transporte_ativo_2008-2019/" 
#data_path <- "data/"
gc(reset=T)
library(ggplot2)
library(readr)
library(data.table)
library(magrittr)
library(patchwork)
library(dplyr)
library(showtext)
library(ggthemes)
library(ipeaplot) # remotes::install_github("ipeadata-lab/ipeaplot")
library(openxlsx)

showtext_auto()
showtext_opts(dpi = 300)




# 1) prop ~ br + metro + sit ----------
rm(list=ls()) 
data_path <- "../../data/transporte_ativo_2008-2019/" 
#data_path <- "data/"
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
single_dt2[region == "Brasil" & ano %in% c(2008,2019),round(100 * mean,2)] 
single_dt2[region == "Rural"] 
single_dt2[region_f == "Metropolitano \nUrbano"]


# plot
p1 <- ggplot(data = single_dt2
       , aes(x = factor(ano)
             , y = mean
             , fill = type)) + 
  geom_bar(stat="identity"
           , color="black"
           , width = .75
  ) +
  scale_y_continuous(labels = scales::percent)+
  ipeaplot::scale_fill_ipea(palette = "Green")+
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
  ) +
  ipeaplot::theme_ipea(legend.position = "bottom")
# theme_classic()+
#   # Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
#   scale_fill_brewer(palette = "Pastel1") +
#   theme(legend.key.width=unit(2,"line"),
#         text = element_text(family = "Times New Roman"),
#         legend.position = "bottom",
#         legend.text = element_text(size = rel(0.8)
#                                    , family = "Times New Roman"
#                                    , face = "plain"),
#         legend.title = element_text(size = rel(0.95)
#                                     , family = "Times New Roman"
#                                     , face = "bold"),
#         title = element_text(size = 10
#                              , family = "Times New Roman"
#                              , face = "plain"),
#         plot.margin=unit(c(0,2,0,1),"mm"),
#         strip.text.x = element_text(size=rel(1.2)),
#         panel.background = element_rect(fill = "white",colour = NA),
#   )

p1

ggsave(p1,
       filename = "figures/1_prop_metro_sit_brasil.png"
       ,width = 15
       ,height = 10
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)

ipeaplot::save_eps(p1,
                   file.name = "figures/1_prop_metro_sit_brasil.eps"
                   ,width = 15
                   ,height = 10
                   ,scale = 1.3
                   ,units = "cm"
                   ,dpi = 300)


# 2) prop ~ sexo + RACA + dummeyMetro----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
#data_path <- "data/"
gc(reset=TRUE)
pnad2008 <- readr::read_rds(paste0(data_path,"export_pnad08/sexo_race_dummyMetro.rds"))
pns13 <- readr::read_rds(paste0(data_path,"export_pns13/sexo_raca_dummyMetro.rds"))
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/sexo_raca_dummyMetro.rds"))

# set DT
pnad2008 <- pnad2008$sexo_race_dummyMetro
pns13 <- pns13$sexo_raca_dummyMetro
pns19 <- pns19$sexo_raca_dummyMetro

data.table::setDT(pnad2008)
data.table::setDT(pns13)
data.table::setDT(pns19)

# rename files
vec_names <- c("sexo","raca","dummyMetro","mean","ci_l","ci_u")
names(pnad2008) <- vec_names
names(pns19) <-  c("raca","sexo","dummyMetro","mean","ci_l","ci_u")
names(pns13) <- c("raca","sexo","dummyMetro","mean","ci_l","ci_u")


# add columns
pnad2008[,":="(ano = 2008)]
pns13[,":="(ano = 2013)]
pns19[,":="(ano = 2019)]

# rbind
single_dt2 <- list(
  # Br files
  pnad2008
  ,pns13
  ,pns19
) %>% data.table::rbindlist(use.names = TRUE) 
single_dt2 <- single_dt2[raca %in% c("Branca","Negra"),]

single_dt2

# fix factors

single_dt2[,label := factor(
  x = paste(sexo,raca)
  ,levels =  c("Feminino Negra"
               ,"Feminino Branca"
               ,"Masculino Negra"
               ,"Masculino Branca")
  ,labels = c("Mulher Negra"
              ,"Mulher Branca"
              ,"Homem Negro"
              ,"Homem Branco")
)]

single_dt2[
  ,raca_f := factor(
    x = raca
    ,levels = c("Branca","Negra")
  )]


single_dt2[
  ,ano_f := factor(
    x = ano
    ,levels = c("2008","2013","2019")
  )]


single_dt2[
  ,sexo_f := factor(
    x = sexo
    ,levels = c("Feminino","Masculino")
    ,labels = c("Mulheres","Homens")
  )]

single_dt2[
  ,dummyMetro_f := factor(
    x = dummyMetro      
    ,levels = c("Metro","Non-metro")
    ,labels = c("Brasil Metropolitano","Brasil Não-Metropolitano")
  )]

single_dt2
# text 
single_dt2[sexo == "Feminino" & ano == "2019"]
single_dt2[sexo == "Masculino" & ano == "2019"]


# plot
p <- ggplot(data = single_dt2
            ,aes(
              y = mean
              , x = ano_f 
              , group = label
            )) + 
  geom_path(aes(
    color = label
  )
  ,position = position_dodge(width = 0)) +
  geom_pointrange(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , color = label
    )
    , show.legend = FALSE
    ,shape = 19) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = label
      , fill = label
    )
    , show.legend = FALSE
    ,alpha = 0.10
    ,position = position_dodge(width = 0)) +
  # text
  geom_text(data = single_dt2[ano %in% c(2008),]
            ,aes(x = ano_f, y = mean, label = round(100*mean,1)
            ),size = 2.25,nudge_x =-.15) +
  geom_text(data = single_dt2[ano %in% c(2019),]
            ,aes(x = ano_f, y = mean, label = round(100*mean,1)
            ),size = 2.25,nudge_x =+.15) +
  facet_grid(rows = vars(dummyMetro_f),cols = vars(sexo_f))+
  scale_y_continuous(labels = scales::percent
                     ,limits = c(0,max(single_dt2$ci_u)))+
  scale_color_manual(values = c(
    '#9e3d2f'
    ,'#fcad5f'
    ,'#2b5c8f'
    ,'#95c5ef'
  ))+
  scale_fill_manual(values = c(
    '#9e3d2f'
    ,'#fcad5f'
    ,'#2b5c8f'
    ,'#95c5ef'
  ))+
  labs(
    #title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Pessoas acima de 18 anos conforme sexo e cor/raça"
    title = NULL, subtitle = NULL
    , x = NULL, y = "Proporção (%)"
    , color = "Sexo e Cor/raça"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  ) +
  ipeaplot::theme_ipea(legend.position = "bottom")

ggsave(p,
       filename = "figures/2_prop_raca_sexo_dummyMetro_values.png"
       ,width = 15
       ,height = 15
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)

ipeaplot::save_eps(gplot = p,
                   file.name = "figures/2_prop_raca_sexo_dummyMetro_values.eps"
                   ,width = 15
                   ,height = 15
                   ,scale = 1.3
                   ,units = "cm"
                   ,dpi = 300)



# 3) prop ~ sexo + ageLarge ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"

gc(reset=TRUE)
pnad2008 <- readr::read_rds(paste0(data_path,"export_pnad08/sexo_ageLarge3.rds"))
pns13 <- readr::read_rds(paste0(data_path,"export_pns13/sexo_ageLarge3.rds"))
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/sexo_ageLarge3.rds"))

# set DT
pnad2008 <- pnad2008$sexo_age
pns13 <- pns13$sexo_age
pns19 <- pns19$sexo_age

data.table::setDT(pnad2008)
data.table::setDT(pns13)
data.table::setDT(pns19)

# rename files
vec_names <- c("agegroup","sexo","mean","ci_l","ci_u")
names(pnad2008) <- vec_names
names(pns13) <- vec_names
names(pns19) <- vec_names

# add columns
pnad2008[,":="(ano = 2008)]
pns13[,":="(ano = 2013)]
pns19[,":="(ano = 2019)]

# rbind
single_dt2 <- list(
  # Br files
  pnad2008
  ,pns13
  ,pns19
) %>% data.table::rbindlist()


# fix factors
# vec_label <- c("18-29",
#                 "30-39",
#                 "40-49",
#                 "50-59",
#                  "60+")

vec_label <- c("18-34",
               "35-54",
               "55+")

table(single_dt2$agegroup)

single_dt2[
  ,agegroup_f := factor(
    x = agegroup
    ,levels = vec_label
  )]


single_dt2[
  ,ano_f := factor(
    x = ano
    ,levels = c("2008","2013","2019")
  )]


single_dt2[
  ,sexo_f := factor(
    x = sexo
    ,levels = c("Feminino","Masculino")
  )]

# text 
single_dt2[sexo == "Masculino" & ano_f == "2008" & agegroup_f == "18-34"] 
single_dt2[sexo == "Masculino" & ano_f == "2019" & agegroup_f == "18-34"]
0.2917469  - 0.2096622 

single_dt2[sexo == "Masculino" & ano_f == "2008" & agegroup_f == "35-54"] 
single_dt2[sexo == "Masculino" & ano_f == "2019" & agegroup_f == "35-54"]
0.3102164 -0.2286989 

single_dt2[sexo == "Feminino" & ano_f == "2008" & agegroup_f == "18-34"] 
single_dt2[sexo == "Feminino" & ano_f == "2019" & agegroup_f == "18-34"]
0.3054509   - 0.2314635  

single_dt2[sexo == "Feminino" & ano_f == "2008" & agegroup_f == "55+"] 
single_dt2[sexo == "Feminino" & ano_f == "2019" & agegroup_f == "55+"]
0.2611987 -0.2312117 

p3 <- 
  ggplot(data = single_dt2
         ,aes(
           y = mean
           , x =  ano_f
           # , group = agegroup_f
         )) + 
  geom_path(aes(
    x = ano_f
    , color = agegroup_f
    , group = agegroup_f
  )
  ,position = position_dodge(width = .1)
  ) +
  facet_wrap(facets = ~sexo_f ,ncol = 2) +
  geom_pointrange(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , color = agegroup_f
      , fill = agegroup_f
      , group = agegroup_f
    )
    , show.legend = FALSE
    ,position = position_dodge(width = .1)
    ,shape = 21) +
  scale_y_continuous(labels = scales::percent
                     , limits = c(0, .32)
                     , breaks = seq(0,0.35,0.05)) +
  labs(
    title = NULL, subtitle = NULL
    , x = "Ano", y = "Proporção (%)"
    , fill = "Faixa etária"
    , color = "Faixa etária"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  ) +
  # theme_bw(base_size = 18)
  ipeaplot::scale_color_ipea(palette = 'Green') +
  ipeaplot::scale_fill_ipea(palette = 'Green') +
  ipeaplot::theme_ipea(legend.position = 'bottom') 

p3

ggsave(p3,
       filename = "./figures/3_prop_idade_sexo_v2.png"
       ,width = 15
       ,height = 9
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)


ipeaplot::save_eps(p3,
                   file.name = "figures/3_prop_idade_sexo_v2.eps"
                   ,width = 15
                   ,height = 10
                   ,scale = 1.3
                   ,units = "cm"
                   ,dpi = 300)








# 4) prop ~ sexo + ESC ----------


# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
#data_path <- "data/"
gc(reset=TRUE)
pnad2008 <- readr::read_rds(paste0(data_path,"export_pnad08/sexo_esc.rds"))
pns13 <- readr::read_rds(paste0(data_path,"export_pns13/sexo_esc.rds"))
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/sexo_esc.rds"))

# set DT
pnad2008 <- pnad2008$sexo_esc
pns13 <- pns13$sexo_esc
pns19 <- pns19$sexo_esc

data.table::setDT(pnad2008)
data.table::setDT(pns13)
data.table::setDT(pns19)

# rename files
vec_names <- c("edugroup","sexo","mean","ci_l","ci_u")
names(pnad2008) <- vec_names
names(pns19) <- vec_names
names(pns13) <- vec_names


# add columns
pnad2008[,":="(ano = 2008)]
pns13[,":="(ano = 2013)]
pns19[,":="(ano = 2019)]

# rbind
single_dt2 <- list(
  # Br files
  pnad2008
  ,pns13
  ,pns19
) %>% data.table::rbindlist(use.names = TRUE)

single_dt2

# fix factors

single_dt2[
  ,edugroup_f := factor(
    x = edugroup
    ,levels = c("Sem instrução + Fundamental incompleto"
                ,"Fundamental completo"
                ,"Médio completo"
                ,"Superior completo")
    ,labels = c("Sem instrução"
                ,"Fundamental completo"
                ,"Médio completo"
                ,"Superior completo")
  )]


single_dt2[
  ,ano_f := factor(
    x = ano
    ,levels = c("2008","2013","2019")
  )]


single_dt2[
  ,sexo_f := factor(
    x = sexo
    ,levels = c("Masculino","Feminino")
  )]

# text
single_dt2[edugroup == "Sem instrução + Fundamental incompleto" & 
             ano == 2019,round(mean * 100 ,1),by = sexo]
single_dt2[edugroup == "Superior completo" & 
             ano == 2019,round(mean * 100 ,1),by = sexo]

# plot
p4 <- ggplot(data = single_dt2
             , aes(
               x = ano_f    
               , y = mean
               , group = edugroup_f 
             )) + 
  geom_path(aes(
    y = mean
    , color = edugroup_f
    , group = edugroup_f
  )
  ,position = position_dodge(width = 0.10)) +
  geom_pointrange(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , color = edugroup_f
      , group = edugroup_f
    )
    ,shape = 19
    , show.legend = FALSE
    ,position = position_dodge(width = 0.10)) +
  geom_ribbon( show.legend = FALSE,
               aes(
                 ymin = ci_l
                 , ymax = ci_u
                 , group = edugroup_f
                 , fill = edugroup_f
               )
               ,alpha = 0.10
               ,position = position_dodge(width = 0.10)) +
  # text
  geom_text(data = single_dt2[ano %in% c(2008),]
            ,aes(x = ano_f, y = mean, label = round(100*mean,1)
            ),size = 2.25,nudge_x =-.2) +
  geom_text(data = single_dt2[ano %in% c(2019),]
            ,aes(x = ano_f, y = mean, label = round(100*mean,1)
            ),size = 2.25,nudge_x =+.2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .41))+
  facet_wrap(facets = ~sexo_f
             ,nrow = 1)+
  labs(
    #title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Escolaridade e sexo"
    title = NULL, subtitle = NULL
    , x = NULL
    , y = "Proporção (%)"
    , color = "Escolaridade"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = guide_legend(
    override.aes = list(linetype = 0))
    , fill = "none")+
  ipeaplot::theme_ipea(legend.position = 'bottom') +
  ipeaplot::scale_color_ipea(palette = 'Orange-Blue-White') +
  ipeaplot::scale_fill_ipea(palette = 'Orange-Blue-White')

p4  

ggsave(p4,
       filename = "figures/4_prop_escolaridade_sexo.png"
       ,width = 16
       ,height = 10
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)


ipeaplot::save_eps(p4,
                   file.name = "figures/4_prop_escolaridade_sexo.eps"
                   ,width = 15
                   ,height = 10
                   ,scale = 1.3
                   ,units = "cm"
                   ,dpi = 300)




# 5) prop ~ quintil + regiao ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
#data_path <- "data/"
pnad2008_br <- readr::read_rds(paste0(data_path,"export_pnad08/br_quint.rds"))
pns13_br <- readr::read_rds(paste0(data_path,"export_pns13/br_quint.rds"))
pns19_br <- readr::read_rds(paste0(data_path,"export_pns19/br_quint.rds"))

pnad2008_regionQuint <- readr::read_rds(paste0(data_path,"export_pnad08/region_quint.rds"))
pns13_regionQuint <- readr::read_rds(paste0(data_path,"export_pns13/region_quint.rds"))
pns19_regionQuint <- readr::read_rds(paste0(data_path,"export_pns19/region_quint.rds"))

# set DT
data.table::setDT(pnad2008_br$br_quint)
data.table::setDT(pns13_br$br_quint_1)
data.table::setDT(pns19_br$br_quint_1)

data.table::setDT(pnad2008_regionQuint$region_quint)
data.table::setDT(pns13_regionQuint$region_quint_1)
data.table::setDT(pns19_regionQuint$region_quint_1)

# rename files
vec_names <- c("region","quintile","mean","ci_l","ci_u")
names(pnad2008_regionQuint$region_quint) <- vec_names
names(pns13_regionQuint$region_quint_1) <- vec_names
names(pns19_regionQuint$region_quint_1) <- vec_names

names(pnad2008_br$br_quint) <- vec_names
names(pns13_br$br_quint_1) <- vec_names
names(pns19_br$br_quint_1) <- vec_names

# add columns
pnad2008_regionQuint$region_quint[,":="(ano = 2008)]
pns13_regionQuint$region_quint_1[,":="(ano = 2013)]
pns19_regionQuint$region_quint_1[,":="(ano = 2019)]

pnad2008_br$br_quint[,":="(ano = 2008)]
pns13_br$br_quint_1[,":="(ano = 2013)]
pns19_br$br_quint_1[,":="(ano = 2019)]

# rbind
single_dt2 <- list(
  # Br files
  pnad2008_br$br_quint
  ,pns13_br$br_quint_1
  ,pns19_br$br_quint_1
  # metro files
  ,pnad2008_regionQuint$region_quint
  ,pns13_regionQuint$region_quint_1
  ,pns19_regionQuint$region_quint_1
  
) %>% data.table::rbindlist()


# fix factors
# single_dt2[,":="(
#   ci_l1 = fifelse(type == "Sim, todo o trajeto",ci_l,ci_l+mean[2])
#   ,ci_u1 = fifelse(type == "Sim, todo o trajeto",ci_u,ci_u+mean[2])
# )
# ,by = .(ano,region)
# ]
# single_dt2[is.na(ci_l1),":="(
#   ci_l1 = ci_l ,
#   ci_u1 = ci_u 
# )]

single_dt2[,quintile := factor(x = quintile
                               ,levels = 1:5
                               ,labels = c("1 (20% mais pobres)"
                                           ,2:4
                                           ,"5 (20% mais ricos)"))]

p5 <- ggplot(data = single_dt2,
       aes(x = factor(ano)
           , y = mean
           , fill = factor(quintile))) + 
  geom_path(aes(x= factor(ano)
                ,y = mean
                , color = factor(quintile)
                , group = factor(quintile))) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = factor(quintile)
      , fill = factor(quintile)
    )
    ,alpha = 0.20
    ,position = position_dodge(width = 0)) +
  scale_y_continuous(labels = scales::percent)+
  geom_pointrange(aes(
    ymin = ci_l
    , ymax = ci_u
    , fill = factor(quintile)
  )
  ,shape = 21) +
  facet_wrap(facets = ~region
             ,nrow = 2)+
  viridis::scale_color_viridis(discrete = TRUE
                               ,option = "D"
                               ,direction = -1
                               ,guide = "none")+
  viridis::scale_fill_viridis(discrete = TRUE
                              ,option = "D"
                              ,direction = -1)+
  labs(
    #title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    title = NULL
    #, subtitle = "Pessoas acima de 18 anos"
    , subtitle = NULL
    , x = NULL, y = "Proporção (%)"
    , fill = "Quintil de\n renda"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  ipeaplot::theme_ipea()

p5

ggsave(p5,
       filename = "figures/5_prop_regiao_quint.png"
       ,width = 15
       ,height = 9
       ,scale = 1.5
       ,units = "cm",bg = "white"
       ,device = "png"
       ,dpi = 300)

ipeaplot::save_eps(p5,
                   file.name = "figures/5_prop_regiao_quint.eps"
                   ,width = 15
                   ,height = 10
                   ,scale = 1.3
                   ,units = "cm"
                   ,dpi = 300)






# 8) prop + 2019 ~ quintil + metro  ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
#data_path <- "data/"
gc(reset=TRUE)


#pns19_mean <- readr::read_rds(paste0(data_path,"export_pns19/"))
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/metro_quint.rds"))

# add columns
pns19 <- pns19$region_quint_1
setDT(pns19)
pns19[,":="(ano = 2019)]

data.table::setnames(pns19,"P040_todo_trajeto","mean")


single_dt2 <- pns19
# fix factors

single_dt2[
  ,ano_f := factor(
    x = ano
    ,levels = c("2008","2013","2019")
  )]

single_dt2[
  ,quintileMetro_f := factor(
    x = quintileMetro
    ,levels =1:5
  )]

metro_order <- single_dt2[quintileMetro == 1] %>% 
  .[order(mean),metro]

single_dt2[
  ,metro_f := factor(
    x = metro
    ,levels = metro_order
  )]


# plot
tmp_plot <- single_dt2[quintileMetro %in% c(1,3,5)
                       # & metro != "Distrito Federal"
                       # & metro != "Belém"
                       # & metro != "Fortaleza"
                       ,]

# text 
values_city <- function(city){
  tmp_plot[metro_f == city,
           paste0(round(mean * 100,1),"% ",quintileMetro,"ºQ")] %>% 
    gsub("\\.","\\,",.) %>% paste0(.,collapse = "; ")
}
values_city("Salvador")
values_city("Belém")
values_city("Recife")
values_city("Fortaleza")

tmp_plot[agegroup_f == "65+" & sexo == "Masculino"]
tmp_plot[agegroup_f == "Masculino" & ano == "2019"]


ggplot() + 
  # data
  geom_errorbar(data = tmp_plot[quintileMetro_f != 3],
                aes(
                  xmin = ci_l
                  , x = mean
                  , y = metro_f
                  , xmax = ci_u
                  , color = quintileMetro_f
                ),width = .65#, #color = "black"
                ,position = position_dodge(width = +0.25)
                ,alpha = 0.5) +
  geom_point(data = tmp_plot,
             aes(x = mean, y = metro_f,fill = quintileMetro_f)
             , size = 3.5
             , shape = 21
             ,position = position_dodge(width = +0.25)
             ,alpha = 1) +
  # text
  geom_text(data = tmp_plot[quintileMetro_f == 3],
            aes(x = mean,y = metro_f
                ,label = paste0(round(100*mean,1),""))
            ,size = 2.35,nudge_y = .32)+
  # scale
  scale_x_continuous(labels = scales::percent)+
  scale_fill_manual(values = c(
    viridisLite::viridis(10)[5], "black",viridisLite::viridis(10)[10]
  )
  ,labels = c("1 \n(20% mais pobre)","3","5 \n(20% mais ricas)"))+
  scale_color_manual(values = c("black","black")
                     ,labels = c("1 \n(20% mais pobre)"
                                 ,"5 \n(20% mais ricas)"))+
  #facet_wrap(~sexo_f,ncol = 2)+
  labs(
    #title = 'Proporção de pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Conforme região metropolitana, sexo, e quintis de renda"
    title = NULL, subtitle = NULL,label = NULL
    , x = "Proporção (%)"
    , y = NULL
    , fill = "Quintil"
    , caption = "Fonte: PNS (2019)"
  )+
  guides(fill = guide_legend(label.position = "bottom"),
         color = "none",label = "none")+
  ipeaplot::theme_ipea(legend.position = "bottom")
#theme_minimal()+     
#theme(legend.position = "bottom",
#      legend.key.width=unit(2,"line"),
#      text = element_text(family = "Times New Roman"),
#      legend.text = element_text(size = rel(0.8)
#                                 , family = "Times New Roman"
#                                 , face = "plain"),
#      legend.title = element_text(size = rel(0.95)
#                                  , family = "Times New Roman"
#                                  , face = "bold"),
#      title = element_text(size = 10
#                           , family = "Times New Roman"
#                           , face = "plain"),
#      plot.margin=unit(c(0,2,0,1),"mm"),
#      strip.text.x = element_text(size=rel(1.2)),
#      panel.grid.major.y = element_line(colour = "grey92"),
#      panel.background = element_rect(fill = "white",colour = NA))


ggsave(filename = "figures/prop_quint_metro.jpg"
       ,width = 12
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

# plot
ggplot(
  data = single_dt2[quintileMetro %in% c(1:5)
                    & metro != "Fortaleza",]
  , aes(x = mean, y = metro_f)
) + 
  geom_point(
    aes(fill = quintileMetro_f)
    , size = 3.5
    , shape = 21
    ,position = position_dodge(width = +0.0)
    ,alpha = 0.75) +
  scale_x_continuous(labels = scales::percent)+
  scale_fill_brewer(direction = -1)+
  scale_color_brewer(direction = -1)+
  facet_wrap(~sexo_f,ncol = 2)+
  labs(
    #title = 'Proporção de pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Conforme região metropolitana, sexo, e quintis de renda"
    title = NULL, subtitle = NULL
    , x = "Proporção (%)"
    , y = NULL
    , fill = "Quintil"
    , caption = "Fonte: PNS (2019)"
  )+
  guides(color = "none")+
  theme_minimal()+   theme(legend.key.width=unit(2,"line"),         text = element_text(family = "Times New Roman"),         legend.text = element_text(size = rel(0.8)                                    , family = "Times New Roman"                                    , face = "plain"),         legend.title = element_text(size = rel(0.95)                                     , family = "Times New Roman"                                     , face = "bold"),         title = element_text(size = 10                              , family = "Times New Roman"                              , face = "plain"),         plot.margin=unit(c(0,2,0,1),"mm"),         strip.text.x = element_text(size=rel(1.2)),         panel.grid.major.y = element_line(colour = "grey92"),         panel.background = element_rect(fill = "white",colour = NA),         legend.box.background = element_rect(fill=alpha('white', 0.7),                                              colour = "#A09C9C",                                              linewidth = 0.5,                                              linetype = "solid"))


ggsave(filename = "figures/prop_quint_metro_sexo1.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

