# Load libraries ----
rm(list=ls())
gc(reset = TRUE)
easypackages::packages('data.table'
                       ,'magrittr'
                       ,'ggplot2'
                       ,'ipeaplot'
                       ,'patchwork')

# remotes::install_github("https://github.com/ipeadata-lab/ipeaplot/tree/d4a956c419025e04d818325da01ba8202b70bfff")
library(showtext)

showtext_auto()
showtext_opts(dpi = 300)

#' @convencoes Pallete
#' @param gender = "Reds"
#' @param idade = "Greens"
#' @param activ = "Blues"
#' @param race = "Purples"

## Figura 7  -----
rm(list=ls())
gc(reset = TRUE)

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

### read deaths data ----------
dados_acid <- readr::read_rds("data/datasus/metro_by_mode_age_cor_sexo.rds")
dados_acid$year %>% unique()
dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
            "Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")
dados_acid <- dados_acid[name_metro %in% rm_pns,]
dados_acid <- dados_acid[!(causa_name %in% c('walk+bike','total'))] %>% 
  .[causa_name %in% c('tric','cami','othe','undetermined'
                      ,'onib'),causa_name := "others"]
dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE)
),by = .(causa_name,year)]

### merge data  ----
dt <- data.table::merge.data.table(
  x = dados_acid_all,y = proj    ,by = "year")
dt[,rel_100k := round(deaths / (pop / 100000),2)]
dt[,year_f := as.factor(gsub("^20","",year))]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"others")
                           ,labels = c("Automóvel","Motocicleta",
                                       "Bicicleta","A pé","Outros"))]
### plot ====
ggplot(dt)+
  geom_point(aes(x = year_f
                 ,y = rel_100k,color = causa_name_f
                 ,group = causa_name_f),size = 1.0)+
  geom_line(aes(x = year_f,color = causa_name_f,
                ,y = rel_100k,group = causa_name_f)
            ,linewidth = 0.85)+
  labs(x = "Ano",y = "Óbitos a cada 100 mil hab."
       ,color = "Modo de transporte"
       ,title = "Óbitos por modo de transporte"
       ,caption = "Total das RM da PNS de 2019. \nFonte: DATASUS.")+
  ipeaplot::theme_ipea(legend.position = "bottom")+
  guides(color=guide_legend(nrow=1,byrow=TRUE,
                            title.position = "top"))
# save
ggsave("figures/datasus/figura7.jpg"
       ,width = 12,height = 10,units = "cm"
       ,dpi = 300,scale = 1.2)


### text analysis -----
copy(dt) %>% 
  .[year %in% c(2011,2021),] %>% 
  .[,{
    rate <- (max(rel_100k) - min(rel_100k))/max(rel_100k)
    list("rate" =100* rate,"max" = max(rel_100k),"min" = min(rel_100k))
  },by = .(causa_name)] %>% 
  .[]

## Figura 8 -----------

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
ibge_2010 <- ibge_2010[,list("pop_rm" = sum(pop,na.rm = TRUE)),by=.(AGE)]
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
),by = .(causa_name,year,AGE)]

dt <- data.table::merge.data.table(
  x = dados_acid_all,y = proj    ,by = "year") %>% 
  data.table::merge.data.table(
    x = .,y = ibge_2010[,.SD,.SDcols = c('AGE','prop_rm')]
    ,by = "AGE")
dt[,pop := pop * prop_rm]
dt[,rel_100k := round(deaths / (pop / 100000),2)]
# filter
dt[,.SD[1],by = c('AGE','year')] # view first obs
dt$causa_name %>% unique()
dt <- dt[!(causa_name %in% c('walk+bike','total'))]
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
tmp_dt <- copy(dt)[causa_name %in% c("walk","bike")]
### plot ====

remotes::install_github('https://github.com/ipeadata-lab/ipeaplot/tree/27d3befbd47501abd285a95093759a2fb9031fb4')

ggplot(data = dt)+
  geom_area(aes(x = year,group = AGE,y = deaths,fill = AGE),
            linewidth = 0.25
            ,color = "black")+
  #scale_fill_brewer(palette = "Greens")+
  facet_wrap(vars(causa_name_f),scales = "free_y",ncol = 2)+
  ipeaplot::theme_ipea(legend.position = "bottom")


labs(x = "Ano",title = "Óbitos totais por faixa etária"
     ,fill = "Faixa etária"
     ,y = "Óbitos a cada \n100 mil habitantes"
     ,caption = "Soma das RM da PNS de 2019. \nFonte: DATASUS.")


ggsave("figures/datasus/obito_100k_idade_walk_bike.jpg"
       ,width = 15,height = 9,units = "cm"
       ,dpi = 300,scale = 1.2)
## Figura 9 -----------

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
ibge_2010 <- ibge_2010[,list("pop_rm" = sum(pop,na.rm = TRUE))
                       ,by = .(sexo)]
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
),by = .(causa_name,year,sexo)]

dt <- data.table::merge.data.table(
  x = dados_acid_all
  ,y = ibge_2010[,.SD,.SDcols = c("sexo","prop_rm")]
  ,by = "sexo"
) %>% 
  data.table::merge.data.table(
    x = .    ,y = proj    ,by = "year")
dt[,pop := pop * prop_rm]
dt[,rel_100k := round(deaths / (pop / 100000),3)]
# filter
dt <- dt[(causa_name %in% c("walk","bike")) & 
           sexo != "Sem declaração",]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("bike","walk")
                           ,labels = c("Bicicleta","A pé"))]
dt[,year_f := as.factor(gsub("^20","",year))]


### plot ====


ggplot(dt)+
  geom_line(aes(x = year_f,group = causa_name_f
                ,y = rel_100k,color = causa_name_f),
            linewidth = 1
            #,color = "black"#,position =  "identity"
  )+
  geom_point(aes(x = year_f,group = causa_name_f
                 ,y = rel_100k,color = causa_name_f),
             size = 2
             #,color = "black"#,position =  "identity"
  )+
  geom_text(data = dt[causa_name == "bike"]
            ,aes(x = year_f
                 ,y = rel_100k,label = round(rel_100k,2)),size = 2.75
            ,angle = 25,nudge_y = 0.05
  )+
  geom_text(data = dt[causa_name == "walk"]
            ,aes(x = year_f
                 ,y = rel_100k,label = round(rel_100k,2)),size = 2.75
            ,angle = 25,nudge_y = 0.45
  )+
  #scale_color_brewer(palette = "Blues")+
  ipeaplot::scale_fill_ipea(palette = "Blue")+
  ggh4x::facet_grid2(rows = vars(causa_name_f)
                     ,cols = vars(sexo)
                     ,scales = "free")+
  labs(x = "Ano",y = "Óbitos a cada
       100 mil habilitantes"
       ,color = "Modo transporte"
       ,title = "Óbitos conforme sexo"
       ,caption = "Soma das RM da PNS de 2011-2021. \nFonte: DATASUS.")+
  ipeaplot::theme_ipea(legend.position = "bottom")

ggsave("figures/datasus/Figura_9.jpg"
       ,width = 15,height = 12,units = "cm"
       ,dpi = 300,scale = 1.2)


## Figura 10  -----------

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
ibge_2010 <- ibge_2010[,list("pop_rm" = sum(pop,na.rm = TRUE))
                       ,by = .(sexo,cor)]
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
),by = .(causa_name,year,sexo,cor)]

dt <- data.table::merge.data.table(
  x = dados_acid_all
  ,y = ibge_2010[,.SD,.SDcols = c("sexo","cor","prop_rm")]
  ,by = c("sexo","cor")
) %>% 
  data.table::merge.data.table(
    x = .    ,y = proj    ,by = "year")
dt[,pop := pop * prop_rm]
dt[,rel_100k := round(deaths / (pop / 100000),2)]
# filter
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("walk+bike")
                           ,labels = c("A pé e bicicleta"))]
dt <- dt[(causa_name %in% c("walk+bike")) & 
           sexo != "Sem declaração" & cor != "Sem declaração",]

dt[,year_f := as.factor(gsub("^20","",year))]

###  plot ----
ggplot(dt[cor %in% c("Branca","Negra"),])+
  geom_line(aes(x = year_f,group = cor
                ,y = rel_100k,color = cor),
            linewidth = 1
            #,color = "black"#,position =  "identity"
  )+
  geom_point(aes(x = year_f,group = cor
                 ,y = rel_100k,color = cor),
             size = 2
             #,color = "black"#,position =  "identity"
  )+
  geom_text(aes(x = year_f
                ,y = fcase(cor == "Branca" & sexo == "Homens"  , 0.4 + rel_100k,
                           cor == "Branca" & sexo == "Mulheres", .4 + rel_100k,
                           cor == "Negra" & sexo == "Homens"  , .4 + rel_100k,
                           cor == "Negra" & sexo == "Mulheres", .4 + rel_100k)
                ,label = round(rel_100k,1)),size = 2.75)+
  #scale_color_brewer(palette = "Blues")+
  ipeaplot::scale_color_ipea(palette = "Blue")+
  ggh4x::facet_grid2(cols = vars(cor)
                     ,rows = vars(sexo)
                     ,scales = "fixed")+
  labs(x = "Ano",y = "Óbitos a cada
       100 mil habilitantes"
       ,color = "Cor ou raça"
       ,title = "Óbitos conforme cor/raça e sexo"
       ,caption = "Soma das RM da PNS de 2011-2021. \nFonte: DATASUS.")+
  ipeaplot::theme_ipea(legend.position = "bottom")

ggsave("figures/datasus/Figura_10.jpg"
       ,width = 15,height = 12,units = "cm"
       ,dpi = 300,scale = 1.2)

### text analysis -----
copy(dt) %>% 
  .[cor %in% c("Branca","Negra"),] %>% 
  .[year %in% c(2011,2021),] %>% 
  .[,{
    rate <- (max(rel_100k) - min(rel_100k))/max(rel_100k)
    list("rate" =100* rate,"max" = max(rel_100k),"min" = min(rel_100k))
  },by = .(cor,sexo)] %>% 
  .[]




# End ----
