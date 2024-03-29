# read data
dados_acid <- readr::read_rds("data/datasus/metro_by_mode_age_cor_sexo.rds")






# filtra soh RMs da PNS
dados_acid[, name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal", 
           name_metro := "Distrito Federal"]

rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")


dados_acid <- dados_acid[ name_metro %in% rm_pns]

# remove unused transport modes
dados_acid <- subset(dados_acid, causa_name %in% c('auto', 'moto', 'bike', 'walk'))

df_all <- dados_acid[, .(deaths = sum(deaths)), 
                     by = .(year, causa_name)]

df_metro <- dados_acid[, .(deaths = sum(deaths)), 
                       by = .(year, name_metro, causa_name)]


df_all[, causa_name_f := factor(causa_name,
                                levels = c("auto","moto","bike","walk" ,"total"),
                                labels = c("Automóvel","Motocicleta", 
                                           "Bicicleta","A pé","Total"))]

df_metro[, causa_name_f := factor(causa_name,
                                  levels = c("auto","moto","bike","walk" ,"total"),
                                  labels = c("Automóvel","Motocicleta", 
                                             "Bicicleta","A pé","Total"))]

ggplot(data=df_all)+
  geom_point(aes(x = year, y = deaths, 
                 color = causa_name, group = causa_name)) +
  geom_line(aes(x = year, y = deaths, 
                color = causa_name, group = causa_name),
            linewidth = 0.25) +
  facet_wrap(vars(causa_name),scales = "free_y")+
  scale_color_ipea(discrete = T, palette = 'Orange-Blue') +
  theme_ipea()


# 1) ALL MODES -----
## 1.1) Ob. / 100k by gender -----

dados_acid <- dados_acid[!is.na(prop)]
dados_acid[,pop := pop * prop]

# filtra soh RMs da PNS
dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

dados_acid <- dados_acid[name_metro %in% rm_pns,]

# soma
dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE)
),by = .(causa_name,year,AGE,sexo)]

pop_rm_year <- dados_acid[,.SD[1],by = .(name_metro,year,AGE,sexo)] %>% 
  .[,list("pop"=sum(pop,na.rm = TRUE)),by = .(year,AGE,sexo)]

dados_acid_all  <- dados_acid_all[pop_rm_year,on = c("year","AGE","sexo")]

dados_acid_all <- dados_acid_all[,rel_100k := round(deaths / (pop / 1000000),2)]

# checking 
dados_acid_all$causa_name %>% table(useNA = "always")
dados_acid_all$year %>% table(useNA = "always")
dados_acid_all$sexo %>% table(useNA = "always")
dados_acid_all$AGE %>% table(useNA = "always")

# filter
dt <- dados_acid_all[!(AGE %in% "0-17") & 
                       causa_name %in% c("walk","bike","total","moto","auto"),]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"total")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Total"))]
dt[,year_f := as.factor(gsub("^20","",year))]


ggplot(dt)+
  geom_point(aes(x = year_f,group = AGE
                 ,y = rel_100k,color = AGE),size = 1.0)+
  geom_line(aes(x = year_f,group = AGE
                ,y = rel_100k,color = AGE),linewidth = 0.7)+
  scale_color_brewer(palette = "Reds")+
  facet_grid(cols = vars(sexo),rows = vars(causa_name_f),
             scales = "free_y")+
  labs(x = "Ano",y = "Obitos a cada 100 mil hab."
       ,color = "Faixa etária"
       ,title = "Óbitos por modo de transporte"
       ,caption = "Média das RM da PNS de 2019.\nFonte: DATASUS.")+
  theme_minimal()+
  theme(legend.position = "right",
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


## 1.2) Ob. / 100k by age & mode -----
rm(list=ls())
gc(reset = TRUE)
dados_acid <- readr::read_rds("data/datasus/metro_by_mode_age_cor_sexo.rds")

# filtra soh RMs da PNS
dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]

rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

dados_acid <- dados_acid[name_metro %in% rm_pns,]

### read proportion ----
ibge_2010 <- readr::read_rds("data-raw/sidrar/censo_2010_RM.rds")
ibge_2010 <- ibge_2010[!is.na(name_metro)]
ibge_2010[,sum(prop),by = .(name_metro)] # should be always one
ibge_2010[,name_metro := gsub("RM ","",name_metro)]
ibge_2010[name_metro %like% "Distrito Federal"
          ,name_metro := "Distrito Federal"]
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

ibge_2010 <- ibge_2010[name_metro %in% rm_pns,]
ibge_2010 <- ibge_2010[,list("pop_rm" = sum(pop,na.rm = TRUE))]
ibge_2010[,prop_rm := pop_rm/sum(pop_rm)]

### read projection of RM region ----
dt_metro <- readr::read_rds("data-raw/sidrar/read_metro_area.rds")
dt_metro <- dt_metro[,.SD,.SDcols = c('code_muni','name_metro')]

proj <- readr::read_rds("data-raw/sidrar/pop_2011_to_2021.rds")
proj <- proj[dt_metro,on = c('municipio_codigo'  = 'code_muni')]
proj <- proj[,list("pop" = sum(valor,na.rm = TRUE)),by = .(year,name_metro)]
proj[,name_metro := gsub("RM ","",name_metro)]
proj[name_metro %like% "Distrito Federal",name_metro := "Distrito Federal"]

rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")
proj <- proj[name_metro %in% rm_pns,]
proj <- proj[,list("pop" = sum(pop,na.rm = TRUE)),by = year]

### merge data  ----
dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE)
),by = .(causa_name,year)]

dt <- data.table::merge.data.table(
  x = dados_acid_all,y = proj    ,by = "year")
dt[,rel_100k := round(deaths / (pop / 100000),2)]
# filter
#dt <- dt[causa_name %in% c(walk+bike)]
dt[
  !(causa_name %in% c('auto','bike','moto','walk'))
  ,causa_name := "outros"
]
dt[,year_f := as.factor(gsub("^20","",year))]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"outros")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Outros"))]
dt[,year_f := as.factor(gsub("^20","",year))]

ggplot(dt)+
  geom_point(aes(x = year_f,group = AGE
                 ,y = rel_100k,color = AGE),size = 1.0)+
  geom_line(aes(x = year_f,group = AGE
                ,y = rel_100k,color = AGE),linewidth = 1.0)+
  scale_color_brewer(palette = "Greens")+
  facet_wrap(vars(causa_name_f),scales = "free_y")+
  labs(x = "Ano",y = "Obitos a cada 100 mil hab."
       ,color = "Faixa etária"
       ,title = "Óbitos por modo de transporte"
       ,caption = "Média das RM da PNS de 2019. \nFonte: DATASUS.")+
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
        plot.margin=unit(c(2,2,0,1),"mm"),
        strip.text.x = element_text(size=rel(1.2)),
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA))


ggsave("figures/datasus/obito_transporte_age.jpg"
       ,width = 17.5,height = 15,units = "cm"
       ,dpi = 300,scale = 1.2)

## 1.22) Ob. / 100k by mode (Geom Area) -----
rm(list=ls())
gc(reset = TRUE)
dados_acid <- readr::read_rds("data/datasus/metro_by_mode_age_cor_sexo.rds")
dados_acid <- dados_acid[!is.na(prop)]
dados_acid[,pop := pop * prop]

# filtra soh RMs da PNS
dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

dados_acid <- dados_acid[name_metro %in% rm_pns,]

# sum by metro
dados_acid[causa_name == "bike" & year %in% c(2013),sum(deaths),by = .(name_metro)]
dados_acid[causa_name == "bike" & year %in% c(2015),sum(deaths),by = .(name_metro)]

unique(dados_acid$causa_name)
# soma
dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE),
  "pop" = sum(pop,na.rm = TRUE)
),by = .(causa_name,year)]

dados_acid_all[,rel_100k := round(deaths / (pop / 1000000),2)]

# checking 
dados_acid_all$causa_name %>% table(useNA = "always")
dados_acid_all$year %>% table(useNA = "always")

# filter 
dt <- dados_acid_all[!(causa_name %in% c("walk+bike","total")),]
dt <- dados_acid_all[!(causa_name %in% c("walk","bike"
                                         ,"moto","auto")),causa_name := "others"]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"others")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Outros"))]
dt[,year_f := as.factor(gsub("^20","",year))]

ggplot(dt)+
  #ggplot(data, ) + 
  geom_area(aes(x=year, y=deaths, fill=group))
#geom_area(aes(x = year_f
#               ,y = rel_100k,color = causa_name_f
#               ,group = causa_name_f),size = 1.0)+
geom_area(aes(x = year_f,fill = causa_name_f
              ,y = rel_100k,group = causa_name_f),
          linewidth = 0.25
          ,color = "black")
geom_line(aes(x = year_f,color = causa_name_f,
              ,y = rel_100k,group = causa_name_f)
          ,linewidth = 0.85)
#scale_color_brewer(palette = "Greens")+
#facet_wrap(vars(causa_name_f))+
#facet_wrap(vars(causa_name_f),scales = "free_y")+
labs(x = "Ano",y = "Óbitos a cada 100 mil hab."
     # y = "Óbitos totais"
     ,color = "Modo de transporte"
     ,title = "Óbitos por modo de transporte"
     ,caption = "Total das RM da PNS de 2019. \nFonte: DATASUS.")+
  ipeaplot::theme_ipea(legend.position = "none")


ggsave("figures/datasus/obito_transporte_total_cumarea.jpg"
       ,width = 17.5,height = 10,units = "cm"
       ,dpi = 300,scale = 1.2)



## 1.3) Ob. totais by age & mode -----------
rm(list=ls())
gc(reset = TRUE)
dados_acid <- readr::read_rds("data/datasus/metro_by_mode_age_cor_sexo.rds")

# filtra soh RMs da PNS
dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

dados_acid <- dados_acid[name_metro %in% rm_pns,]

dados_acid[name_metro == "São Paulo"]

dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE)
),by = .(causa_name,year,AGE)]

pop_rm_year <- dados_acid[,.SD[1],by = .(name_metro,year,AGE)] %>% 
  .[,list("pop"=sum(pop,na.rm = TRUE)),by = .(year,AGE)]

dados_acid_all  <- dados_acid_all[pop_rm_year,on = c("year","AGE")]

dt <- dados_acid_all[AGE != "Sem declaração" & 
                       causa_name %in% c("walk","bike"
                                         ,"total","moto","auto"),]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"total")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Total"))]
dt[,year_f := as.factor(gsub("^20","",year))]

dt
ggplot(dt)+
  geom_area(aes(x = year_f,group = AGE
                ,y = deaths,fill = AGE),
            linewidth = 0.25
            ,color = "black")+
  scale_fill_brewer(palette = "Greens")+
  facet_wrap(vars(causa_name_f),scales = "free_y")+
  labs(x = "Ano",y = "Obitos totais"
       ,fill = "Faixa etária"
       ,title = "Óbitos por modo de transporte"
       ,caption = "Soma das RM da PNS de 2019. \nFonte: DATASUS.")+
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
        plot.margin=unit(c(2,2,0,1),"mm"),
        strip.text.x = element_text(size=rel(1.2)),
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA))


ggsave("figures/datasus/obito_totais_transporte.jpg"
       ,width = 12.5,height = 10,units = "cm"
       ,dpi = 300,scale = 1.2)

### Proportion ----
ggplot(dt)+
  geom_area(aes(x = year_f,group = AGE
                ,y = deaths,fill = AGE),
            linewidth = 0.25
            ,color = "black",position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = "Greens")+
  facet_wrap(vars(causa_name_f),scales = "fixed")+
  labs(x = "Ano",y = "Proporção de óbitos totais (%)"
       ,fill = "Faixa etária"
       ,title = "Óbitos por modo de transporte"
       ,caption = "Soma das RM da PNS de 2019. \nFonte: DATASUS.")+
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


ggsave("figures/datasus/obito_totais_transporte_proportion.jpg"
       ,width = 12.5,height = 10,units = "cm"
       ,dpi = 300,scale = 1.2)
### plot 2 - total ====
ggplot(dt[causa_name %in% c("walk","bike")])+
  geom_area(aes(x = year_f,group = causa_name_f
                ,y = rel_100k,fill = causa_name_f),
            linewidth = 0.25
            ,color = "black")+
  scale_fill_brewer(palette = "Greens")+
  facet_wrap(vars(AGE),scales = "free_y",ncol = 2)+
  labs(x = "Ano",title = "Óbitos totais por faixa etária"
       ,fill = "Modo transporte"
       ,y = "Óbitos a cada \n100 mil habitantes"
       ,caption = "Soma das RM da PNS de 2019. \nFonte: DATASUS.")+
  ipeaplot::theme_ipea(legend.position = "bottom")

ggsave("figures/datasus/obito_100k_idade_ativ.jpg"
       ,width = 12,height = 15,units = "cm"
       ,dpi = 300,scale = 1.2)


## 1.5) Ob. totais by facet(year) -----------
rm(list=ls())
gc(reset = TRUE)
dados_acid <- readr::read_rds("data/datasus/metro_by_mode_age_cor_sexo.rds")

# filtra soh RMs da PNS
dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

dados_acid <- dados_acid[name_metro %in% rm_pns,]


dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE)
),by = .(causa_name,year,AGE)]

pop_rm_year <- dados_acid[,.SD[1],by = .(name_metro,year,AGE)] %>% 
  .[,list("pop"=sum(pop,na.rm = TRUE)),by = .(year,AGE)]

dados_acid_all  <- dados_acid_all[pop_rm_year,on = c("year","AGE")]

dados_acid_all <- dados_acid_all[,rel_100k := round(deaths / (pop / 1000000),2)]

dt <- dados_acid_all[AGE != "Sem declaração" & 
                       (causa_name %in% c("walk","bike","total"
                                          ,"moto","auto")),]
dt[,AGE_f := factor(AGE
                    ,levels = sort(unique(dt$AGE))
                    ,labels = gsub("-","-\n",sort(unique(dt$AGE))))]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"total")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Total"))]
dt[,year_f := as.factor(gsub("^20","",year))]


ggplot(dt[causa_name != "total"])+
  geom_col(aes(x = AGE
               ,y = deaths,fill = causa_name_f),
           linewidth = 0.25
           ,color = "black",position =  "identity")+
  scale_fill_brewer(palette = "Greens")+
  facet_wrap(vars(year),scales = "free_y")+
  labs(x = "Idade",y = "Obitos totais"
       ,fill = "Modo transporte"
       ,title = "Óbitos por ano"
       ,caption = "Soma das RM da PNS de 2019. \nFonte: DATASUS.")+
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
        panel.background = element_rect(fill = "white",colour = NA),
        axis.text.x = element_text(angle = 45
                                   ,hjust = 0.75
                                   ,vjust = 0.75))

ggsave("figures/datasus/obito_totais_ano.jpg"
       ,width = 15,height = 15,units = "cm"
       ,dpi = 300,scale = 1.2)

## 1.6) Ob. totais by gender & age -----------
rm(list=ls())
gc(reset = TRUE)
dados_acid <- readr::read_rds("data/datasus/metro_by_mode_age_cor_sexo.rds")

dados_acid[year == 2019 & causa_name == "bike"
           & sexo == "Mulheres",.N,by = .(AGE)]

# filtra soh RMs da PNS
dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

dados_acid <- dados_acid[name_metro %in% rm_pns,]

dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE),
  "pop" = pop[1]
),by = .(causa_name,year,AGE,sexo)]

dados_acid_all[1:5]
dados_acid_all[causa_name == "bike"]


dt <- dados_acid_all[causa_name != "total" & 
                       AGE != "Sem declaração" & 
                       sexo != "Sem declaração" & 
                       causa_name %in% c("walk","bike"
                                         ,"total","moto","auto"),]
dt[,AGE_f := factor(AGE
                    ,levels = sort(unique(dt$AGE))
                    ,labels = gsub("-","-\n",sort(unique(dt$AGE))))]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"total")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Total"))]
dt[,year_f := as.factor(gsub("^20","",year))]


p_plot <- function(sex,legend,
                   position = "identity",my_caption = NULL,
                   x = NULL,y = NULL){
  
  p1 <- ggplot(dt[sexo == sex])+
    geom_col(aes(x = AGE
                 ,y = deaths,fill = causa_name_f),
             linewidth = 0.25
             ,width = .75
             ,color = "black"
             ,position = position)+
    scale_fill_brewer(palette = "Reds")+
    facet_grid(rows = vars(year)
               ,cols = vars(sexo))+
    labs(x = x,y = y
         ,fill = "Modo transporte"
         ,title = NULL
         ,caption = my_caption)+
    theme_minimal()+
    theme(legend.position = legend,
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
          panel.background = element_rect(fill = "white",colour = NA),
          axis.text.x = element_text(angle = 45
                                     ,hjust = 0.75
                                     ,vjust = 0.75))+
    guides(fill=guide_legend(nrow=2,byrow=TRUE,
                             title.position = "top"))
  
  if(sex == "Homens"){
    p1 <- p1 + theme(strip.text.y = element_blank())
  }
  #if(sex == "Mulheres" & position == "fill"){
  #  p1 <- p1 + theme(axis.text.y = element_blank()
  #                   ,axis.ticks.y = element_blank())
  #}
  if(position == "fill"){
    p1 <- p1 +  
      scale_y_continuous(labels = scales::percent)
    
  }
  return(p1)
}


pf <- p_plot("Homens","none"
             ,my_caption = NULL
             ,y = "Obitos Totais") +
  p_plot("Mulheres","bottom"
         ,my_caption ="Soma das RM da PNS de 2019. \nFonte: DATASUS."
         ,y = NULL)

pf

ggsave(plot = pf,"figures/datasus/obito_totais_ano_sexo.jpg"
       ,width = 15,height = 20,units = "cm"
       ,dpi = 300,scale = 1.2)

### Proportion -----
dt[,deaths := 100 * deaths / sum(deaths,na.rm = TRUE),by = .(AGE,sexo,year)]

pf <- p_plot("Homens","none",position = "fill"
             ,my_caption = NULL
             ,y = "Obitos Totais") +
  p_plot("Mulheres","bottom",position = "fill"
         ,my_caption ="Soma das RM da PNS de 2019. \nFonte: DATASUS."
         ,y = NULL)

pf
ggsave("figures/datasus/obito_totais_ano_sexo_proportion.jpg"
       ,width = 15,height = 20,units = "cm"
       ,dpi = 300,scale = 1.2)

## 1.7) Ob. totais by race & gender -----------
rm(list=ls())
gc(reset = TRUE)
dados_acid <- readr::read_rds("data/datasus/metro_by_mode_age_cor_sexo.rds")

# filtra soh RMs da PNS
dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

dados_acid <- dados_acid[name_metro %in% rm_pns,]


dados_acid <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE)
),by = .(causa_name,year,sexo,cor)]

dt <- dados_acid[causa_name %in% c("auto","moto","bike","walk") & 
                   cor %in% c("Branca","Negra") &
                   sexo != "Sem declaração",]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"total")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Total"))]
dt[,year_f := as.factor(gsub("^20","",year))]


ggplot(dt)+
  geom_col(aes(x = year_f,group = causa_name_f
               ,y = deaths,fill = causa_name_f),
           linewidth = 0.25
           ,color = "black",position =  "stack")+
  #scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = "Greens")+
  ggh4x::facet_grid2(rows = vars(sexo)
                     ,cols = vars(cor))+
  labs(x = "Ano",y = "Obitos totais"
       ,fill = "Modo transporte"
       ,title = "Óbitos conforme sexo e raça"
       ,caption = "Soma das RM da PNS de 2011-2021. \nFonte: DATASUS.")+
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
        plot.margin=unit(c(2,2,0,1),"mm"),
        strip.text.x = element_text(size=rel(1.2)),
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.background = element_rect(fill = "white",colour = NA),
        axis.text.x = element_text(angle = 0
                                   ,hjust = 0.75
                                   ,vjust = 0.75))

ggsave("figures/datasus/obito_sexo_cor_total.jpg"
       ,width = 15,height = 12,units = "cm"
       ,dpi = 300,scale = 1.2)

### Proportion -----
ggplot(dt)+
  geom_col(aes(x = factor(year_f)
               ,y = deaths,fill = causa_name_f),
           linewidth = 0.25
           ,color = "black",position =  "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = "Greens")+
  ggh4x::facet_grid2(rows = vars(sexo)
                     ,cols = vars(cor)
                     ,scales = "free")+
  labs(x = "Ano",y = "Obitos totais"
       ,fill = "Modo transporte"
       ,title = "Óbitos conforme sexo e raça"
       ,caption = "Soma das RM da PNS de 2011-2021. \nFonte: DATASUS.")+
  theme_minimal()+
  theme(legend.position = "bottom"
        ,panel.grid.major.x =  element_blank()
        #,panel.grid.minor.y =  element_blank()
        ,panel.grid.major.y =  element_line(colour="grey92"
                                            ,linetype = "dotted"
                                            , linewidth = 0.25),
        text = element_text(family = "Times New Roman"),
        legend.text = element_text(size = rel(0.8), face = "plain"),
        legend.title = element_text(size = rel(0.95), face = "bold"),
        title = element_text(size = 10, face = "plain"),
        plot.margin=unit(c(2,2,0,1),"mm"),
        strip.text.x = element_text(size=rel(1.2)),
        panel.background = element_rect(fill = "white",colour = NA))

ggsave("figures/datasus/obito_sexo_cor_proportion.jpg"
       ,width = 15,height = 12,units = "cm"
       ,dpi = 300,scale = 1.2)


### Proportion -----
dt <- data.table::merge.data.table(
  x = dados_acid_all
  ,y = ibge_2010[,.SD,.SDcols = c("sexo","cor","prop_rm")]
  ,by = c("sexo","cor")
) %>% 
  data.table::merge.data.table(
    x = .    ,y = proj    ,by = "year")
dt[,pop := pop * prop_rm]
dt[,rel_100k := round(deaths / (pop / 1000000),2)]

# filter
dt$causa_name %>% unique()
dt <- dt[!(causa_name %in% c('walk+bike','total'))]
dt[!(causa_name %in% c("auto","moto","bike","walk"))
   ,causa_name := "outros"]
dt
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"outros")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Outros"))]

dt <- dt[sexo != "Sem declaração" & cor %in% c("Branca","Negra"),]

dt[,year_f := as.factor(gsub("^20","",year))]

ggplot(dt[cor %in% c('Branca', 'Negra') & causa_name != "total"])+
  geom_col(aes(x = factor(year_f)
               ,y = rel_100k,fill = causa_name_f),
           linewidth = 0.25
           ,color = "black")+
  #scale_y_continuous(labels = scales::percent)+
  #scale_color_brewer(palette = "Blues")+
  ipeaplot::scale_fill_ipea(palette = "Red-Blue-White")+
  ggh4x::facet_grid2(rows = vars(sexo)
                     ,cols = vars(cor)
                     ,scales = "free")+
  labs(x = "Ano"
       ,y = "Óbitos a cada
       100 mil habilitantes"
       #, y = "Proporção (%)"
       ,fill = "Cor ou rança"
       ,title = "Óbitos conforme sexo e raça"
       ,caption = "Soma das RM da PNS de 2011-2021. \nFonte: DATASUS.")+
  ipeaplot::theme_ipea(legend.position = "bottom")


ggsave("figures/datasus/activ_sexo_cor_proportion.jpg"
       ,width = 15,height = 12,units = "cm"
       ,dpi = 300,scale = 1.2)

ggplot(dt[cor %in% c('Branca', 'Negra') & causa_name != "total"])+
  geom_col(aes(x = factor(year_f)
               ,y = deaths,fill = causa_name_f),
           linewidth = 0.25
           ,color = "black",position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  #scale_color_brewer(palette = "Blues")+
  ipeaplot::scale_fill_ipea(palette = "Red-Blue-White")+
  ggh4x::facet_grid2(rows = vars(sexo)
                     ,cols = vars(cor)
                     ,scales = "free")+
  labs(x = "Ano"
       #,y = "Óbitos a cada
       #100 mil habilitantes"
       , y = "Proporção (%)"
       ,fill = "Cor ou rança"
       ,title = "Óbitos conforme sexo e raça"
       ,caption = "Soma das RM da PNS de 2011-2021. \nFonte: DATASUS.")+
  ipeaplot::theme_ipea(legend.position = "bottom")


ggsave("figures/datasus/all-modes_sexo_cor_proportion.jpg"
       ,width = 15,height = 12,units = "cm"
       ,dpi = 300,scale = 1.2)

# End -----