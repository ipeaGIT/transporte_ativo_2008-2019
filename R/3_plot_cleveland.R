rm(list=ls())
gc(reset=T)
library(ggplot2)
library(readr)
library(data.table)
library(magrittr)
library(patchwork)
library(ipeaplot) # remotes::install_github("ipeadata-lab/ipeaplot")



# 1) prop ~ quintil + regiao ----------

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

ggplot(data = single_dt2,
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
  theme_minimal()+
  theme(legend.key.width=unit(2,"line"),
        text = element_text(family = "Times New Roman"),
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
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA))


ggsave(filename = "figures/prop_regiao_quint.jpg"
       ,width = 15
       ,height = 9
       ,scale = 1.5
       ,units = "cm"
       ,dpi = 300)
# 1) prop ~ quintil + Metro ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
data_path <- "data/"
pnad2008_br <- readr::read_rds(paste0(data_path,"export_pnad08/br_quint.rds"))
pns13_br <- readr::read_rds(paste0(data_path,"export_pns13/br_quint.rds"))
pns19_br <- readr::read_rds(paste0(data_path,"export_pns19/br_quint.rds"))

pnad2008_regionQuint <- readr::read_rds(paste0(data_path,"export_pnad08/metro_quint.rds"))
pns13_regionQuint <- readr::read_rds(paste0(data_path,"export_pns13/metro_quint.rds"))
pns19_regionQuint <- readr::read_rds(paste0(data_path,"export_pns19/metro_quint.rds"))

# set DT
data.table::setDT(pnad2008_br$br_quint)
data.table::setDT(pns13_br$br_quint_1)
data.table::setDT(pns19_br$br_quint_1)

data.table::setDT(pnad2008_regionQuint$metro_quint)
data.table::setDT(pns13_regionQuint$metro_quint_1)
data.table::setDT(pns19_regionQuint$region_quint_1)

# rename files
vec_names <- c("region","quintile","mean","ci_l","ci_u")
names(pnad2008_regionQuint$metro_quint) <- vec_names
names(pns13_regionQuint$metro_quint_1) <- vec_names
names(pns19_regionQuint$region_quint_1) <- vec_names

names(pnad2008_br$br_quint) <- vec_names
names(pns13_br$br_quint_1) <- vec_names
names(pns19_br$br_quint_1) <- vec_names

# add columns
pnad2008_regionQuint$metro_quint[,":="(ano = 2008)]
pns13_regionQuint$metro_quint_1[,":="(ano = 2013)]
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
  ,pnad2008_regionQuint$metro_quint
  ,pns13_regionQuint$metro_quint_1
  ,pns19_regionQuint$region_quint_1
  
) %>% data.table::rbindlist()

single_dt2[,quintile := factor(x = quintile
                               ,levels = 1:5
                               ,labels = c("1 (20% mais pobres)"
                                           ,2:4
                                           ,"5 (20% mais ricos)"))]

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

ggplot(data = single_dt2,
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
             ,nrow = 4)+
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
  theme_minimal()+  
  theme(legend.position = "bottom",
        legend.key.width=unit(2,"line"),
        text = element_text(family = "Times New Roman"),
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
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA))



ggsave(filename = "figures/prop_metro_quint.jpg"
       ,width = 15
       ,height = 18
       ,scale = 1.5
       ,units = "cm"
       ,dpi = 300)

# 2) prop ~ sexo + age ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
data_path <- "data/"
gc(reset=TRUE)
pnad2008 <- readr::read_rds(paste0(data_path,"export_pnad08/sexo_age.rds"))
pns13 <- readr::read_rds(paste0(data_path,"export_pns13/sexo_age.rds"))
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/sexo_age.rds"))

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
vec_label <- c("18-24", "25-29" ,"30-34", "35-39", "40-44"
               , "45-49", "50-54", "55-59", "60-64", "65+")

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


##  plot v1 ----
ggplot(data = single_dt2
       ,aes(
         x = agegroup_f
         , y = mean
         , group = ano_f
       )) + 
  geom_path(aes(
    y = mean
    , color = ano_f
    , group = ano_f
  )
  ,position = position_dodge(width = 0)) +
  geom_pointrange(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , color = ano_f
      , fill = ano_f
      , group = ano_f
    )
    ,position = position_dodge(width = 0)
    #, width = .75
    ,shape = 21
  ) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = ano_f
      , fill = ano_f
    )
    ,alpha = 0.10
    ,position = position_dodge(width = 0.0)) +
  scale_y_continuous(labels = scales::percent
                     ,breaks = seq(0,0.35,by = 0.05))+
  facet_wrap(facets = ~sexo_f
             ,nrow = 2)+
  #viridis::scale_color_viridis(discrete = TRUE
  #                             ,option = "D"
  #                             ,direction = -1
  #                             ,guide = "none")+
  #viridis::scale_fill_viridis(discrete = TRUE
  #                            ,option = "D"
  #                            ,direction = -1)+
  scale_color_manual(values = c(
    '#620C1A'
    ,'#111F4F'
    ,'#C29365'
    ,'#6A9BB3'
  ))+
  scale_fill_manual(values = c(
    '#620C1A'
    ,'#111F4F'
    ,'#C29365'
    ,'#6A9BB3'
  ))+
  labs(
    #title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Pessoas acima de 18 anos conforme sexo e idade"
    , x = "Faixa etária", y = "Proporção (%)"
    , fill = "Ano"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = "none")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.key.width=unit(2,"line"),
        text = element_text(family = "Times New Roman"),
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
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA))


ggsave(filename = "figures/prop_idade_sexo.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

## plot v2----

ggplot(data = single_dt2
       ,aes(
         x = agegroup_f
         , y = mean
         , group = ano_f
       )) + 
  geom_path(aes(
    y = mean
    , color = ano_f
    , group = ano_f
  )
  ,position = position_dodge(width = 0)) +
  geom_pointrange(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , color = ano_f
      , fill = ano_f
      , group = ano_f
    )
    ,position = position_dodge(width = 0)
    #, width = .75
    ,shape = 21
  ) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = ano_f
      , fill = ano_f
    )
    ,alpha = 0.10
    ,position = position_dodge(width = 0.0)) +
  scale_y_continuous(labels = scales::percent
                     ,breaks = seq(0,0.35,0.05))+
  facet_wrap(facets = ~sexo_f
             ,nrow = 2)+
  #viridis::scale_color_viridis(discrete = TRUE
  #                             ,option = "D"
  #                             ,direction = -1
  #                             ,guide = "none")+
  #viridis::scale_fill_viridis(discrete = TRUE
  #                            ,option = "D"
  #                            ,direction = -1)+
  scale_color_manual(values = c(
    '#620C1A'
    ,'#111F4F'
    ,'#C29365'
    ,'#6A9BB3'
  ))+
  scale_fill_manual(values = c(
    '#620C1A'
    ,'#111F4F'
    ,'#C29365'
    ,'#6A9BB3'
  ))+
  labs(
    title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Pessoas acima de 18 anos conforme sexo e idade"
    , x = "Faixa etária", y = "Proporção (%)"
    , fill = "Ano"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = "none")+
  theme_minimal()+   
  theme(legend.position = "bottom",
        legend.key.width=unit(2,"line"),
        text = element_text(family = "Times New Roman"),
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
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA))

ggsave(filename = "figures/prop_idade_sexo.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
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
single_dt2[agegroup_f == "60+"]
single_dt2[agegroup_f == "55+" & sexo == "Masculino"]
single_dt2[sexo == "Masculino" & ano_f == "2019"]



p4 <- 
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

showtext_auto()
showtext_opts(dpi = 300)

ggsave(p4,
       filename = "./figures/prop_idade_sexo_v2.png"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)



png("./figures/prop_idade_sexo_v222.png", width = 15,height = 12, units = "cm", res=300)
print(p4)
dev.off()


# 4) prop ~ sexo + RACA ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
data_path <- "data/"
gc(reset=TRUE)
pnad2008 <- readr::read_rds(paste0(data_path,"export_pnad08/sexo_race.rds"))
pns13 <- readr::read_rds(paste0(data_path,"export_pns13/sexo_raca.rds"))
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/sexo_race.rds"))

# set DT
pnad2008 <- pnad2008$sexo_race
pns13 <- pns13$sexo_raca
pns19 <- pns19$sexo_race_1

data.table::setDT(pnad2008)
data.table::setDT(pns13)
data.table::setDT(pns19)

# rename files
vec_names <- c("sexo","raca","mean","ci_l","ci_u")
names(pnad2008) <- vec_names
names(pns19) <- vec_names
names(pns13) <- c("raca","sexo","mean","ci_l","ci_u")


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
) %>% data.table::rbindlist(use.names = TRUE) %>% 
  .[raca %in% c("Branca","Negra"),]

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


single_dt2
# text 
single_dt2[sexo == "Feminino" & ano == "2019"]
single_dt2[sexo == "Masculino" & ano == "2019"]


# plot
ggplot(data = single_dt2
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
    ,position = position_dodge(width = 0)
    ,shape = 19) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = label
      , fill = label
    )
    ,alpha = 0.10
    ,position = position_dodge(width = 0)) +
  facet_wrap(~sexo_f)+
  scale_y_continuous(labels = scales::percent
                     ,limits = c(0,max(single_dt2$ci_u)))+
  scale_color_manual(values = c(
    '#620C1A'
    ,'#C29365'
    ,'#111F4F'
    ,'#6A9BB3'
  ))+
  scale_fill_manual(values = c(
    '#620C1A'
    ,'#C29365'
    ,'#111F4F'
    ,'#6A9BB3'
  ))+
  labs(
    #title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Pessoas acima de 18 anos conforme sexo e cor/raça"
    title = NULL, subtitle = NULL
    , x = NULL, y = "Proporção (%)"
    , color = "Sexo e Cor/raça"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = guide_legend(
    override.aes = list(linetype = 0))
    , fill = "none")+
  theme_minimal()+  
  theme(legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        text = element_text(family = "Times New Roman"),
        legend.text = element_text(size = rel(1)
                                   , family = "Times New Roman"
                                   , face = "plain"),
        legend.title = element_text(size = rel(1)
                                    , family = "Times New Roman"
                                    , face = "bold"),
        axis.text = element_text(size=rel(1.1)),
        plot.margin=unit(c(0,2,0,1),"mm"),
        strip.text.x = element_text(size=rel(1.2)),
        panel.background = element_rect(fill = "white",colour = NA))


ggsave(filename = "figures/prop_raca_sexo.jpg"
       ,width = 15
       ,height = 9
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)

# 5) prop ~ sexo + ESC ----------
# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
data_path <- "data/"
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
    ,levels = c("Sem instrução + Fundamental incompleto","Médio completo"
                ,"Fundamental completo","Superior completo")
    ,labels = c("Sem instrução","Médio completo"
                ,"Fundamental completo","Superior completo")
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
ggplot(data = single_dt2
       ,aes(
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
    ,position = position_dodge(width = 0.10)) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = edugroup_f
      , fill = edugroup_f
    )
    ,alpha = 0.10
    ,position = position_dodge(width = 0.10)) +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(facets = ~sexo_f
             ,nrow = 2)+
  #scale_color_manual(values = c(
  #  '#620C1A'
  #  ,'#111F4F'
  #  ,'#C29365'
  #  ,'#6A9BB3'
  #))+
  #scale_fill_manual(values = c(
  #  '#620C1A'
  #  ,'#111F4F'
  #  ,'#C29365'
  #  ,'#6A9BB3'
#))+
viridis::scale_color_viridis(discrete = TRUE,alpha = 1,option = "H")+
  viridis::scale_fill_viridis(discrete = TRUE,alpha = 1,option = "H")+
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
  theme_minimal()+   
  theme(legend.key.width=unit(2,"line"),
        text = element_text(family = "Times New Roman"),
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
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA))



ggsave(filename = "figures/prop_escolaridade_sexo.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

# 6) prop ~ raca + ESC ----------


# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
data_path <- "data/"
gc(reset = TRUE)
pnad2008 <- readr::read_rds(paste0(data_path,"export_pnad08/raca_esc.rds"))
pns13 <- readr::read_rds(paste0(data_path,"export_pns13/raca_esc.rds"))
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/raca_esc.rds"))

# set DT
pnad2008 <- pnad2008$raca_esc
pns13 <- pns13$raca_esc
pns19 <- pns19$raca_esc

data.table::setDT(pnad2008)
data.table::setDT(pns13)
data.table::setDT(pns19)

# rename files
vec_names <- c("edugroup","raca_group","mean","ci_l","ci_u")
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

single_dt2 <- single_dt2[raca_group %in% c("Branca","Negra"),]

# fix factors

single_dt2[
  ,edugroup_f := factor(
    x = edugroup
    ,levels = c("Sem instrução + Fundamental incompleto","Médio completo"
                ,"Fundamental completo","Superior completo")
    ,labels = c("Sem instrução","Médio completo"
                ,"Fundamental completo","Superior completo")
  )]


single_dt2[
  ,ano_f := factor(
    x = ano
    ,levels = c("2008","2013","2019")
  )]


single_dt2[
  ,raca_group_f := factor(
    x = raca_group
    ,levels = c("Branca","Negra")
  )]


single_dt2

# plot
ggplot(data = single_dt2[!is.na(raca_group),]
       ,aes(
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
    ,position = position_dodge(width = 0.10)) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = edugroup_f
      , fill = edugroup_f
    )
    ,alpha = 0.10
    ,position = position_dodge(width = 0.10)) +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(facets = ~raca_group_f
             ,nrow = 2)+
  scale_color_manual(values = c(
    '#620C1A'
    ,'#111F4F'
    ,'#C29365'
    ,'#6A9BB3'
  ))+
  scale_fill_manual(values = c(
    '#620C1A'
    ,'#111F4F'
    ,'#C29365'
    ,'#6A9BB3'
  ))+
  labs(
    #title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Escolaridade e cor/raça"
    title = NULL, subtitle = NULL
    , x = NULL
    , y = "Proporção (%)"
    , color = "Escolaridade"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = guide_legend(
    override.aes = list(linetype = 0))
    , fill = "none")+
  theme_minimal()+   theme(legend.key.width=unit(2,"line"),         text = element_text(family = "Times New Roman"),         legend.text = element_text(size = rel(0.8)                                    , family = "Times New Roman"                                    , face = "plain"),         legend.title = element_text(size = rel(0.95)                                     , family = "Times New Roman"                                     , face = "bold"),         title = element_text(size = 10                              , family = "Times New Roman"                              , face = "plain"),         plot.margin=unit(c(0,2,0,1),"mm"),         strip.text.x = element_text(size=rel(1.2)),         panel.grid.major.y = element_line(colour = "grey92"),         panel.background = element_rect(fill = "white",colour = NA),         legend.box.background = element_rect(fill=alpha('white', 0.7),                                              colour = "#A09C9C",                                              linewidth = 0.5,                                              linetype = "solid"))

ggsave(filename = "figures/prop_escolaridade_raca.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

# 7) prop ~ tempo + sexo ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
data_path <- "data/"
gc(reset = TRUE)
pnad2008 <- readr::read_rds(paste0(data_path,"export_pnad08/time_dummy.rds"))
pns13 <- readr::read_rds(paste0(data_path,"export_pns13/time_dummy.rds"))
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/time_dummy.rds"))

# set DT
pnad2008 <- data.table::rbindlist(
  list(  setDT(pnad2008$time_00to09)[,":="( time = "00to09", mean = actv_commutetime_00to09)][, actv_commutetime_00to09 := NULL]
         ,setDT(pnad2008$time_10to19)[,":="( time = "10to19", mean = actv_commutetime_10to19)][, actv_commutetime_10to19 := NULL]
         ,setDT(pnad2008$time_20to29)[,":="( time = "20to29", mean = actv_commutetime_20to29)][, actv_commutetime_20to29 := NULL]
         ,setDT(pnad2008$time_30to44)[,":="( time = "30to44", mean = actv_commutetime_30to44)][, actv_commutetime_30to44 := NULL]
         ,setDT(pnad2008$time_45to59)[,":="( time = "45to59", mean = actv_commutetime_45to59)][, actv_commutetime_45to59 := NULL])
)

pns13    <- data.table::rbindlist(
  list(  setDT(pns13$time_00to09)[,":="( time = "00to09", mean = actv_commutetime_00to09)][, actv_commutetime_00to09 := NULL]
         ,setDT(pns13$time_10to19)[,":="( time = "10to19", mean = actv_commutetime_10to19)][, actv_commutetime_10to19 := NULL]
         ,setDT(pns13$time_20to29)[,":="( time = "20to29", mean = actv_commutetime_20to29)][, actv_commutetime_20to29 := NULL]
         ,setDT(pns13$time_30to44)[,":="( time = "30to44", mean = actv_commutetime_30to44)][, actv_commutetime_30to44 := NULL]
         ,setDT(pns13$time_45to59)[,":="( time = "45to59", mean = actv_commutetime_45to59)][, actv_commutetime_45to59 := NULL]
  )
)

pns19    <- data.table::rbindlist( 
  list( setDT(pns19$time_00to09)[,":="( time = "00to09", mean = actv_commutetime_00to09)][, actv_commutetime_00to09 := NULL]
        ,setDT(pns19$time_10to19)[,":="( time = "10to19", mean = actv_commutetime_10to19)][, actv_commutetime_10to19 := NULL]
        ,setDT(pns19$time_20to29)[,":="( time = "20to29", mean = actv_commutetime_20to29)][, actv_commutetime_20to29 := NULL]
        ,setDT(pns19$time_30to44)[,":="( time = "30to44", mean = actv_commutetime_30to44)][, actv_commutetime_30to44 := NULL]
        ,setDT(pns19$time_45to59)[,":="( time = "45to59", mean = actv_commutetime_45to59)][, actv_commutetime_45to59 := NULL]
  )
)

# rbind
single_dt2 <- list(
  pnad2008[,":="(ano = 2008)]
  ,pns13[,":="(ano = 2013)]
  ,pns19[,":="(ano = 2019)]
) %>% data.table::rbindlist()

single_dt2
# fix factors

single_dt2$time %>% unique()

single_dt2[
  ,time_f := factor(
    x = time
    ,levels = c("00to09","10to19","20to29","30to44","45to59")
    ,labels = c("00 a 09","10 a 19","20 a 29","30 a 44","45 a 59")
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


single_dt2
single_dt2[ano==2019]
single_dt2[ano==2008]

# plot
ggplot(single_dt2
       ,aes(x= time_f 
            ,y = mean)) + 
  geom_path(aes(x= time_f 
                ,y = mean
                , color = sexo_f
                , group = sexo_f)
            ,size = .85) +
  geom_point(aes(x= time_f 
                 ,y = mean
                 , color = sexo_f
                 , group = sexo_f)
             ,size = 1.75) +
  geom_pointrange(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , color = sexo_f
    )
    ,position = position_dodge(width = 0)
    ,shape = 19) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = sexo_f
      , fill = sexo_f
    )
    ,alpha = 0.10
    ,position = position_dodge(width = 0)) +
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c(
    '#620C1A'
    #,'#111F4F'
    ,'#C29365'
    #,'#6A9BB3'
  ))+
  scale_fill_manual(values = c(
    '#620C1A'
    #,'#111F4F'
    ,'#C29365'
    #,'#6A9BB3'
  ))+
  facet_wrap(~ano_f,ncol = 1)+
  labs(
    #title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Pessoas acima de 18 anos"
    title = NULL, subtitle = NULL
    , x = "Intervalo de tempo (minutos)", y = "Proporção (%)"
    , color = "Sexo"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = guide_legend(
    override.aes = list(linetype = 0))
    , fill = "none")+
  theme_minimal()+   theme(legend.key.width=unit(2,"line"),         text = element_text(family = "Times New Roman"),         legend.text = element_text(size = rel(0.8)                                    , family = "Times New Roman"                                    , face = "plain"),         legend.title = element_text(size = rel(0.95)                                     , family = "Times New Roman"                                     , face = "bold"),         title = element_text(size = 10                              , family = "Times New Roman"                              , face = "plain"),         plot.margin=unit(c(0,2,0,1),"mm"),         strip.text.x = element_text(size=rel(1.2)),         panel.grid.major.y = element_line(colour = "grey92"),         panel.background = element_rect(fill = "white",colour = NA),         legend.box.background = element_rect(fill=alpha('white', 0.7),                                              colour = "#A09C9C",                                              linewidth = 0.5,                                              linetype = "solid"))


ggsave(filename = "figures/prop_time_sexo.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

ggplot(single_dt2
       ,aes(x= time_f 
            ,y = mean)) + 
  geom_path(aes(x= time_f 
                ,y = mean
                , color = sexo_f
                , group = sexo_f)
            ,size = .85) +
  geom_point(aes(x= time_f 
                 ,y = mean
                 , color = sexo_f
                 , group = sexo_f)
             ,size = 2.75) +
  geom_pointrange(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , color = sexo_f
    )
    ,position = position_dodge(width = 0)
    ,shape = 19) +
  geom_ribbon(
    aes(
      ymin = ci_l
      , ymax = ci_u
      , group = sexo_f
      , fill = sexo_f
    )
    ,alpha = 0.10
    ,position = position_dodge(width = 0)) +
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c(
    '#620C1A'
    ,'#111F4F'
    ,'#C29365'
    ,'#6A9BB3'
  ))+
  scale_fill_manual(values = c(
    '#620C1A'
    ,'#111F4F'
    ,'#C29365'
    ,'#6A9BB3'
  ))+
  facet_wrap(~ano_f,ncol = 1)+
  labs(
    #title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    #, subtitle = "Pessoas acima de 18 anos"
    title = NULL, subtitle = NULL
    , x = "Intervalo de tempo (minutos)", y = "Proporção (%)"
    , color = "Sexo"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = guide_legend(
    override.aes = list(linetype = 0))
    , fill = "none")+
  theme_minimal()+   theme(legend.key.width=unit(2,"line"),         text = element_text(family = "Times New Roman"),         legend.text = element_text(size = rel(0.8)                                    , family = "Times New Roman"                                    , face = "plain"),         legend.title = element_text(size = rel(0.95)                                     , family = "Times New Roman"                                     , face = "bold"),         title = element_text(size = 10                              , family = "Times New Roman"                              , face = "plain"),         plot.margin=unit(c(0,2,0,1),"mm"),         strip.text.x = element_text(size=rel(1.2)),         panel.grid.major.y = element_line(colour = "grey92"),         panel.background = element_rect(fill = "white",colour = NA),         legend.box.background = element_rect(fill=alpha('white', 0.7),                                              colour = "#A09C9C",                                              linewidth = 0.5,                                              linetype = "solid"))

ggsave(filename = "figures/prop_time_sexo1.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

# 8) prop + 2019 ~ quintil + metro  ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
data_path <- "data/"
gc(reset=TRUE)


pns19_mean <- readr::read_rds(paste0(data_path,"export_pns19/"))
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
    title = NULL, subtitle = NULL
    , x = "Proporção (%)"
    , y = NULL
    , fill = "Quintil"
    , caption = "Fonte: PNS (2019)"
  )+
  guides(fill = guide_legend(label.position = "bottom"),
         color = "none")+
  theme_minimal()+     
  theme(legend.position = "bottom",
        legend.key.width=unit(2,"line"),
        text = element_text(family = "Times New Roman"),
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
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA))


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


# end-----

