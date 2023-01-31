
# Load packages -----
# remotes::install_github("rfsaldanha/microdatasus")
rm(list=ls())


easypackages::packages('data.table'
                       ,'magrittr'
                       ,'ggplot2'
                       ,'microdatasus'
                       ,'basedosdados')

# download data
state <- c('RO','AC','AM','RR','PA','AP','TO','MA'
           ,'PI','CE','RN','PB','PE','AL','SE'
           ,'BA','MG','ES','RJ','SP','PR','SC'
           ,'RS','MS','MT','GO','DF')

# download acid-------
# dados_ext <- microdatasus::fetch_datasus(year_start = 2015
#                                          , year_end = 2019
#                                          , information_system = "SIM-DOEXT")
# readr::write_rds(dados_ext,"data-raw/datasus/SIM-DOEXT_2015_to_2019.rds"
#                  ,compress = "gz")
dados_ext <- readr::read_rds("data-raw/datasus/SIM-DOEXT_2015_to_2019.rds")
dados_inf <- microdatasus::fetch_datasus(year_start = 2015
                                         , year_end = 2019
                                         , information_system = "SIM-DOINF")
setDT(dados_ext)
dados_inf <- dados_ext[1,CAUSABAS_O := "lala"]
setDT(dados_inf)

# apenas acidentes
dados1_ext <- dados_ext[grepl("^V",CAUSABAS_O),]
dados1_inf <- dados_inf[grepl("^V",CAUSABAS_O),]

# remover acidentes que nao sao de transporte terrestre
str_acid <- c(paste0("V0",c(90:97)),paste0("V",c(90:97)))
for(i in str_acid){
  dados1_ext <- dados1_ext[!(CAUSABAS_O %like% i)]
  dados1_inf <- dados1_inf[!(CAUSABAS_O %like% i)]
}


# by type of vehicle
my_list <- list("walk" = paste0("V0",1:9),
                "bike" = paste0("V",10:19),
                "moto" = paste0("V",20:29),
                "tric" = paste0("V",30:39),
                "auto" = paste0("V",40:59),
                "cami" = paste0("V",60:69),
                "onib" = paste0("V",70:79),
                "othe" = paste0("V",c(80:89,98:99)))

for(i in names(my_list)){
  vars <- my_list[i][[1]]
  for(j in vars){
    dados1_ext[CAUSABAS_O %like% j,causa_name := i]
    dados1_inf[CAUSABAS_O %like% j,causa_name := i]
    dados1_inf[CAUSABAS %like% j,causa_name := i]
    dados1_inf[CAUSABAS %like% j,causa_name := i]
  }
}

dados1_ext[is.na(causa_name)]
dados1_ext$causa_name %>% table(.,useNA = "always")
dados1_inf$causa_name %>% table(.,useNA = "always")


diff <- as.Date("08102019","%d%m%Y") - as.Date("08051991","%d%m%Y")
age <- round(as.numeric(diff)/365,0)

# date ext and int
dados1_ext[,date_nasc := as.Date(DTNASC,"%d%m%Y")]
dados1_ext[,date := as.Date(DTOBITO,"%d%m%Y")]
dados1_ext[,year := format(date,"%Y")]
dados1_ext[,age := date - date_nasc]
dados1_ext[,age := round(as.numeric(age)/365,0)]
dados1_ext[,sexo := fcase(SEXO == 1,"Homens",
                          SEXO == 2,"Mulheres")]

dados1_inf[,date_nasc := as.Date(DTNASC,"%d%m%Y")]
dados1_inf[,date := as.Date(DTOBITO,"%d%m%Y")]
dados1_inf[,year := format(date,"%Y")]
dados1_inf[,age :=  date - date_nasc]
dados1_inf[,age := round(as.numeric(age)/365,0)]
dados1_inf[,sexo := fcase(SEXO == 1,"Homens",
                          SEXO == 2,"Mulheres")]

dados_acid <- rbind(
  dados1_ext[,.N,by = .(CODMUNOCOR,causa_name,year,age,sexo)],
  dados1_inf[,.N,by = .(CODMUNOCOR,causa_name,year,age,sexo)])

dados_acid <- dados_acid[,list("num_acid" = sum(N))
                         ,by = .(CODMUNOCOR,causa_name,year,age,sexo)]
setnames(dados_acid,"CODMUNOCOR","code_muni_sus")

dados_acid[age >=0 &  age<18, AGE :="0-17"]
dados_acid[age >17 &  age<25, AGE :="18-24"]
dados_acid[age >=25 & age<35, AGE :="25-34"]
dados_acid[age >=35 & age<45, AGE :="35-44"]
dados_acid[age >=45 & age<55, AGE :="45-54"]
dados_acid[age >=55 & age<65, AGE :="55-64"]
dados_acid[age >=65 & age<74, AGE :="65-74"]
dados_acid[age >=75 & age<99, AGE :="75-99"]

# download pop proj 2019 -------

sidrar::info_sidra(6579)

#dt_pop1 <- sidrar::get_sidra(x = 6579
#                             , period = as.character(2015:2019)
#                             , variable = 9324
#                             , geo = "City")
# readr::write_rds(dt_pop1,"data-raw/sidrar/pop_2015_to_2019.rds")

dt_pop1 <- readr::read_rds("data-raw/sidrar/pop_2015_to_2019.rds")
setDT(dt_pop1)
names(dt_pop1) <- janitor::make_clean_names(names(dt_pop1))
dt_pop1[,code_muni_sus := stringr::str_sub(string = municipio_codigo,start = 1,end = 6)]
setnames(dt_pop1,"ano","year")
dt_pop <- dt_pop1
dt_pop <- dt_pop[,.SD,.SDcols = c("municipio_codigo","code_muni_sus","valor","year")]
dt_pop[1,]

# metro region ----

dt_metro <- geobr::read_metro_area()
setDT(dt_metro)
dt_metro[,code_muni_sus := stringr::str_sub(string = code_muni,start = 1,end = 6)]
dt_metro <- dt_metro[,.SD,.SDcols = c("code_muni_sus","name_metro")]
dt_metro 


# merge data ----

dados_acid1 <- data.table::merge.data.table(
  x = dados_acid  ,y = dt_metro  ,by = c('code_muni_sus')
) %>% data.table::merge.data.table(
  x = .  ,y = dt_pop  ,by = c('code_muni_sus','year')
)

tmp_pop_rm <- copy(dados_acid1)[,.SD[1],by = .(name_metro, municipio_codigo,year)]
tmp_pop_rm <- tmp_pop_rm[,list(pop = sum(valor)),by = .(name_metro,year)]


dados_acid2 <- rbind(dados_acid1[,list(
  "deaths" = sum(num_acid,na.rm = TRUE))
  ,by = .(name_metro,causa_name,year,AGE,sexo)],
  dados_acid1[causa_name %in% c('walk','bike'),list(
    "deaths" = sum(num_acid,na.rm = TRUE))
    ,by = .(name_metro,year,AGE,sexo)][,causa_name := "walk+bike"],
  dados_acid1[,list("deaths" = sum(num_acid,na.rm = TRUE))
              ,by = .(name_metro,year,AGE,sexo)][,causa_name := "total"]
)

dados_acid2 <- dados_acid2[tmp_pop_rm,on = c("name_metro","year")]
dados_acid2[name_metro == "RM Porto Velho",]

# censo 2010 ---- 

ibge_2010 <- readr::read_rds("../RHA/data/IBGE_estrutura_etaria.rds")
order_age <- unique(ibge_2010$idade)
ibge_2010[idade %in% order_age[1:4]  , AGE :="0-17"]
ibge_2010[idade %in% order_age[5:6]  , AGE :="18-24"]
ibge_2010[idade %in% order_age[7:8]  , AGE :="25-34"]
ibge_2010[idade %in% order_age[9:10] , AGE :="35-44"]
ibge_2010[idade %in% order_age[11:12], AGE :="45-54"]
ibge_2010[idade %in% order_age[13:14], AGE :="55-64"]
ibge_2010[idade %in% order_age[15:16], AGE :="65-74"]
ibge_2010[idade %in% order_age[17:19], AGE :="75-99"]

ibge_2010$situacao_do_domicilio %>% unique()
ibge_2010$sexo %>% unique()
ibge_2010$unidade_de_medida %>% unique()

dt_metro <- geobr::read_metro_area()
setDT(dt_metro)
dt_metro[,code_muni := as.character(code_muni)]

ibge_2010 <- ibge_2010[dt_metro, on = c('municipio_codigo'= 'code_muni'),
                       name_metro := i.name_metro]
ibge_2010 <- ibge_2010[!is.na(name_metro),]
ibge_2010 <- ibge_2010[,list("pop" = sum(valor,na.rm = TRUE))
                       ,by = .(name_metro,AGE,sexo)]
ibge_2010[,prop := pop/sum(pop,na.rm = TRUE),by = name_metro]
ibge_2010[is.na(prop),]

# dados acid----

dados_acid2[ibge_2010,on = c("name_metro","AGE","sexo"),prop := i.prop]
dados_acid2[is.na(prop)]

# save data ----

readr::write_rds(dados_acid2,"data/datasus/deaths_roads_metro_historical_age_sexo.rds")


# Explore -----

rm(list=ls())
dados_acid <- readr::read_rds("data/datasus/deaths_roads_metro_historical_age_sexo.rds")
dados_acid <- dados_acid[!is.na(prop)]
dados_acid[,pop := pop * prop]

dados_acid[,name_metro := gsub("RM ","",name_metro)]
dados_acid[name_metro %like% "Distrito Federal"
           ,name_metro := "Distrito Federal"]

# filtra soh RMs da PNS
rm_pns <- c("Belém","Fortaleza","Recife","Salvador","Belo Horizonte",
"Rio de Janeiro","São Paulo","Curitiba","Porto Alegre","Distrito Federal")

dados_acid <- dados_acid[name_metro %in% rm_pns,]

#
dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths)
),by = .(causa_name,year,AGE,sexo)]

pop_rm_year <- dados_acid[,.SD[1],by = .(name_metro,year,AGE,sexo)] %>% 
  .[,list("pop"=sum(pop)),by = .(year,AGE,sexo)]

dados_acid_all  <- dados_acid_all[pop_rm_year,on = c("year","AGE","sexo")]

dados_acid_all <- dados_acid_all[,rel_100k := round(deaths / (pop / 1000000),2)]

dt <- dados_acid_all[!(AGE %in% "0-17") & 
  causa_name %in% c("walk","bike","total","moto","auto"),]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"total"))]
ggplot(dt)+
  geom_point(aes(x = as.numeric(gsub("^20","",year))
                ,y = rel_100k,color = AGE),size = 1.0)+
  geom_line(aes(x = as.numeric(gsub("^20","",year))
                ,y = rel_100k,color = AGE),size = 1.0)+
  scale_color_brewer(palette = "Reds")+
  facet_grid(cols = vars(sexo),rows = vars(causa_name_f),
             scales = "free_y")+
  labs(x = "Ano",y = "Obitos a cada 100 mil hab."
       ,color = "Faixa etária"
       ,title = "Óbitos por modo de transporte"
       ,caption = "Média das RM da PNS de 2019.Fonte: DataSus")+
  theme_bw()

# Explore (I) -----


rm(list=ls())
gc(reset = TRUE)
dados_acid <- readr::read_rds("data/datasus/deaths_roads_metro_historical_age.rds")
dados_acid <- dados_acid[!is.na(prop)]
dados_acid[,pop := pop * prop]

dados_acid[name_metro == "RM São Paulo"]
dados_acid_all <- dados_acid[,list(
  "deaths" = sum(deaths)
),by = .(causa_name,year,AGE)]

pop_rm_year <- dados_acid[,.SD[1],by = .(name_metro,year,AGE)] %>% 
  .[,list("pop"=sum(pop)),by = .(year,AGE)]

dados_acid_all  <- dados_acid_all[pop_rm_year,on = c("year","AGE")]

dados_acid_all <- dados_acid_all[,rel_100k := round(deaths / (pop / 1000000),2)]

dt <- dados_acid_all[!(AGE %in% "0-17") & 
                       causa_name %in% c("walk","bike","total","moto","auto"),]
dt[,causa_name_f := factor(causa_name
                           ,levels = 
                             c("auto","moto","bike","walk"
                               ,"total"))]
ggplot(dt)+
  geom_point(aes(x = as.numeric(gsub("^20","",year))
                 ,y = rel_100k,color = AGE),size = 1.0)+
  geom_line(aes(x = as.numeric(gsub("^20","",year))
                ,y = rel_100k,color = AGE),size = 1.0)+
  scale_color_brewer(palette = "Reds")+
  facet_wrap(vars(causa_name_f),scales = "free_y")+
  labs(x = "Ano",y = "Obitos a cada 100 mil hab."
       ,color = "Faixa etária"
       ,title = "Óbitos por modo de transporte"
       ,caption = "Média das RM da PNS de 2019.Fonte: DataSus")+
  theme_bw()

# End ---