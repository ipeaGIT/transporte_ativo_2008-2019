# load and read -----



99999

# sexo x idade -> fazer só urbana, dodge= 0, cor ~ tons cinza/azul
# brasil+urban/rural+metro/n-metro -> patchwork, colocar legenda embaixo
regionMetro+quintilMetro 
sexo+quintil+metro, para 2019
testar geom_ribbon() para CI


rm(list=ls())
gc(reset=T)
library(ggplot2)
library(readr)
library(data.table)
library(magrittr)
library(patchwork)



# 1) prop ~ br + metro ----------

# read files
rm(list=ls())
pnad2008_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/br_quint.rds")
pns13_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/br_quint.rds")
pns19_br <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/br_quint.rds")

pnad2008_regionQuint <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/region_quint.rds")
pns13_regionQuint <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/region_quint.rds")
pns19_regionQuint <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/region_quint.rds")

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

ggplot(data = single_dt2,
       aes(x = factor(ano)
           , y = mean
           , fill = factor(quintile))) + 
  geom_path(aes(x= factor(ano)
                ,y = mean
                , color = factor(quintile)
                , group = factor(quintile))) +
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
    title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Pessoas acima de 18 anos"
    , x = NULL, y = "Proporção (%)"
    , fill = "Quintil de\n renda"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  theme_minimal()


ggsave(filename = "figures/prop_regiao_quint.jpg"
       ,width = 15
       ,height = 9
       ,scale = 1.5
       ,units = "cm"
       ,dpi = 300)

# 2) prop ~ sexo + age ----------

# read files
rm(list=ls())
pnad2008 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/sexo_age.rds")
pns13 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/sexo_age.rds")
pns19 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/sexo_age.rds")

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


# plot
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
  scale_y_continuous(labels = scales::percent)+
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
  theme_minimal()


ggsave(filename = "figures/prop_idade_sexo.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

# 3) prop ~ sexo + RACA ----------

# read files
rm(list=ls())
pnad2008 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/sexo_race.rds")
pns13 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/sexo_raca.rds")
pns19 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/sexo_race.rds")

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
  scale_y_continuous(labels = scales::percent)+
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
    title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Pessoas acima de 18 anos conforme sexo e cor/raça"
    , x = NULL, y = "Proporção (%)"
    , color = "Sexo e Cor/raça"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = guide_legend(
    override.aes = list(linetype = 0))
    , fill = "none")+
  theme_minimal()+
  theme(legend.position = "bottom")


ggsave(filename = "figures/prop_raca_sexo.jpg"
       ,width = 15
       ,height = 9
       ,scale = 1.3
       ,units = "cm"
       ,dpi = 300)

# 3) prop ~ sexo + ESC ----------


sexo x escolar - > fill = educacao, wrap ~ sexo, alpha = 0.6, dodge = 0
# read files
rm(list=ls())
pnad2008 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/sexo_esc.rds")
pns13 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/sexo_esc.rds")
pns19 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/sexo_esc.rds")

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


single_dt2

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
    , subtitle = "Escolaridade e sexo"
    , x = NULL
    , y = "Proporção (%)"
    , color = "Escolaridade"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
c
  theme_minimal()

ggsave(filename = "figures/prop_escolaridade_sexo.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

# 3) prop ~ tempo + sexo ----------

# read files
rm(list=ls())
pnad2008 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pnad08/time_dummy.rds")
pns13 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns13/time_dummy.rds")
pns19 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/time_dummy.rds")

# set DT
pnad2008 <- data.table::rbindlist(
  list(  setDT(pnad2008$time_00to09)[,time := "00to09"]
        ,setDT(pnad2008$time_10to19)[,time := "10to19"]
        ,setDT(pnad2008$time_20to29)[,time := "20to29"]
        ,setDT(pnad2008$time_30to44)[,time := "30to44"]
        ,setDT(pnad2008$time_45to59)[,time := "45to59"])
)
data.table::setnames(pnad2008,old = "actv_commutetime_00to09",new = "mean") 

pns13    <- data.table::rbindlist(
  list(  setDT(pns13$time_00to09)[,time := "00to09"]
        ,setDT(pns13$time_10to19)[,time := "10to19"]
        ,setDT(pns13$time_20to29)[,time := "20to29"]
        ,setDT(pns13$time_30to44)[,time := "30to44"]
        ,setDT(pns13$time_45to59)[,time := "45to59"]
  )
)
data.table::setnames(pns13,old = "actv_commutetime_00to09",new = "mean") 

pns19    <- data.table::rbindlist( 
  list( setDT(pns19$time_00to09)[,time := "00to09"]
       ,setDT(pns19$time_10to19)[,time := "10to19"]
       ,setDT(pns19$time_20to29)[,time := "20to29"]
       ,setDT(pns19$time_30to44)[,time := "30to44"]
       ,setDT(pns19$time_45to59)[,time := "45to59"]
  )
)
data.table::setnames(pns19,old = "actv_commutetime_00to09",new = "mean") 

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
    title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Pessoas acima de 18 anos"
    , x = "Intervalo de tempo (minutos)", y = "Proporção (%)"
    , color = "Sexo"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = guide_legend(
    override.aes = list(linetype = 0))
    , fill = "none")+
  theme_minimal()


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
    title = 'Proporção das pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Pessoas acima de 18 anos"
    , x = "Intervalo de tempo (minutos)", y = "Proporção (%)"
    , color = "Ano"
    , caption = "Fonte: PNAD (2008), PNS (2013 e 2019)"
  )+
  guides(color = guide_legend(
    override.aes = list(linetype = 0))
    , fill = "none")+
  theme_minimal()

ggsave(filename = "figures/prop_time_sexo1.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)

# 3) prop + 2019 ~ quintil + metro + sexo ----------

# read files
rm(list=ls())
pns19 <- readr::read_rds("../../data/transporte_ativo_2008-2019/export_pns19/sexo_metro_quint.rds")

# add columns
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
  ,sexo_f := factor(
    x = sexo
    ,levels = c("Feminino","Masculino")
  )]

single_dt2[
  ,quintileMetro_f := factor(
    x = quintileMetro
    ,levels =1:5
  )]

metro_order <- single_dt2[quintileMetro == 1 & 
                            sexo == "Feminino",] %>% 
  .[order(mean),metro]

single_dt2[
  ,metro_f := factor(
    x = metro
    ,levels = metro_order
  )]


# plot
ggplot(
  data = single_dt2[quintileMetro %in% c(1,3,5)
                    & metro != "Fortaleza",]
  , aes(x = mean, y = metro_f)
  ) + 
  geom_errorbar(
    aes(
      xmin = ci_l
      , xmax = ci_u
      , color = factor(quintileMetro_f)
    ),width = .75
    ,position = position_dodge(width = +0.0)) +
  geom_point(
    aes(fill = quintileMetro_f)
    , size = 3.5
    , shape = 21
    ,position = position_dodge(width = +0.0)) +
  scale_x_continuous(labels = scales::percent)+
  scale_fill_manual(
    values = c(
      '#620C1A'
      ,'#C29365'
      ,'#6A9BB3'
    ))+
  scale_color_manual(
    values = c(
      '#620C1A'
      ,'#C29365'
      ,'#6A9BB3'
    ))+
  facet_wrap(~sexo_f,ncol = 2)+
  labs(
    title = 'Proporção de pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Conforme região metropolitana, sexo, e quintis de renda"
    , x = "Proporção (%)"
    , y = NULL
    , fill = "Quintil"
    , caption = "Fonte: PNS (2019)"
  )+
  guides(color = "none")+
  theme_minimal()


ggsave(filename = "figures/prop_quint_metro_sexo.jpg"
       ,width = 15
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
    title = 'Proporção de pessoas que se deslocam a pé ou de bicicleta'
    , subtitle = "Conforme região metropolitana, sexo, e quintis de renda"
    , x = "Proporção (%)"
    , y = NULL
    , fill = "Quintil"
    , caption = "Fonte: PNS (2019)"
  )+
  guides(color = "none")+
  theme_minimal()


ggsave(filename = "figures/prop_quint_metro_sexo1.jpg"
       ,width = 15
       ,height = 12
       ,scale = 1.2
       ,units = "cm"
       ,dpi = 300)


# end-----

