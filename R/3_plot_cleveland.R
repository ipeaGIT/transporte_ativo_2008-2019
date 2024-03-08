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





# 2) prop ~ sexo + age ----------

# read files
rm(list=ls())
data_path <- "../../data/transporte_ativo_2008-2019/"
#data_path <- "data/"
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







# 4b) prop ~ sexo + Metro----------


data_path <- "../../data/transporte_ativo_2008-2019/"
pns19 <- readr::read_rds(paste0(data_path,"export_pns19/sexo_raca_Metro.rds"))
pns19 <- rbindlist(pns19)

pns19 |>
  mutate(metro = forcats::fct_reorder(metro, P040_todo_trajeto)) |>
  ggplot() +
  geom_point(aes(y=metro, x=P040_todo_trajeto, color=sexo)) +
  geom_pointrange(aes(y=metro, x=P040_todo_trajeto, 
                      xmin = ci_l, xmax = ci_u, color=sexo),
                  position = position_dodge(width = 0.2)) +
  labs(x= 'Proporção (%)', y = 'Região Metropolitana'
       , color = "Sexo"
       , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c('#620C1A','#111F4F'))+
  geom_text(data = pns19[sexo %in% c('Masculino'),],
            aes(x = P040_todo_trajeto, y = metro, color=sexo,
                label = round(P040_todo_trajeto*100,1)),
            size = 2.25,nudge_y =+.2,nudge_x = .0) +
  geom_text(data = pns19[sexo %in% c('Feminino'),],
            aes(x = P040_todo_trajeto, y = metro, color=sexo,
                label = round(P040_todo_trajeto*100,1)),
            size = 2.25,nudge_y =-.2, nudge_x = .0) +
  
  ipeaplot::theme_ipea(legend.position = "bottom")


ggsave(filename = "figures/prop_raca_sexo_Metro_values.png"
       ,width = 15
       ,height = 15
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)


pns19_wide <- copy(pns19)
pns19_wide[, c('ci_l', 'ci_u') := NULL]

pns19_wide <- data.table::dcast(pns19_wide, metro~ sexo, value.var = 'P040_todo_trajeto')

pns19_wide[, sex_ratio := Feminino / Masculino]


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




# end-----

