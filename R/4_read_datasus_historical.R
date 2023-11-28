# Load packages -----
rm(list=ls())

# remotes::install_github("rfsaldanha/microdatasus")
#  devtools::install_github("danicat/read.dbc")
easypackages::packages('data.table'
                       ,'magrittr'
                       ,'ggplot2'
                       ,'microdatasus')

# 1) download acid-------
## 2011 - 2012 ----
file_datasus11_12 <- "data-raw/datasus/SIM-DOEXT_2011_to_2012.rds"
if(!file.exists(file_datasus11_12)){
  dados_ext <- microdatasus::fetch_datasus(year_start = 2013
                                           , year_end = 2013
                                           , information_system = "SIM-DOEXT")
  readr::write_rds(dados_ext,file_datasus11_12,compress = "gz")
}
## 2013 - 2014 ----
file_datasus13_14 <- "data-raw/datasus/SIM-DOEXT_2013_to_2014.rds"
if(!file.exists(file_datasus13_14)){
  dados_ext <- microdatasus::fetch_datasus(year_start = 2013
                                           , year_end = 2014
                                           , information_system = "SIM-DOEXT")
  readr::write_rds(dados_ext,file_datasus13_14,compress = "gz")
}
file_INFdatasus13_14 <- "data-raw/datasus/SIM-DOINF_2013_to_2014.rds"
if(!file.exists(file_INFdatasus13_14)){
  dados_inf <- microdatasus::fetch_datasus(year_start = 2013
                                           , year_end = 2014
                                           , information_system = "SIM-DOINF")
  readr::write_rds(dados_inf,file_INFdatasus13_14,compress = "gz")
}
## 2015 - 2019 ----
file_datasus15_19 <-"data-raw/datasus/SIM-DOEXT_2015_to_2019.rds"
if(!file.exists(file_datasus15_19)){
  dados_ext <- microdatasus::fetch_datasus(year_start = 2015
                                           , year_end = 2019
                                           , information_system = "SIM-DOEXT")
  readr::write_rds(dados_ext,file_datasus15_19,compress = "gz")
}
file_INFdatasus15_19 <-"data-raw/datasus/SIM-DOINF_2015_to_2019.rds"
if(!file.exists(file_INFdatasus15_19)){
  dados_inf <- microdatasus::fetch_datasus(year_start = 2015
                                           , year_end = 2019
                                           , information_system = "SIM-DOINF")
  readr::write_rds(dados_inf,file_INFdatasus15_19,compress = "gz")
}
## 2020 - 2021 ----
file_EXTdatasus <-"data-raw/datasus/SIM-DOEXT_2020_to_2021.rds"
if(!file.exists(file_EXTdatasus)){
  dados_ext <- microdatasus::fetch_datasus(year_start = 2020
                                           , year_end = 2021
                                           , information_system = "SIM-DOEXT")
  readr::write_rds(dados_ext,file_EXTdatasus,compress = "gz")
}
file_INFdatasus <- "data-raw/datasus/SIM-DOINF_2020_to_2021.rds"
if(!file.exists(file_INFdatasus15_19)){
  dados_inf <- microdatasus::fetch_datasus(year_start = 2020
                                           , year_end = 2021
                                           , information_system = "SIM-DOINF")
  readr::write_rds(dados_inf,file_INFdatasus,compress = "gz")
}


# 2) Read acid ----- 
filter_datasus <- function(filepath){
  #filepath = "data-raw/datasus/SIM-DOINF_2013_to_2014.rds"
  #filepath = "data-raw/datasus/SIM-DOEXT_2015_to_2019.rds"
  dt <- readr::read_rds(filepath)
  data.table::setDT(dt)
  
  # create unique column
  dt[,CAUSABAS := as.character(CAUSABAS)]
  dt[,CAUSABAS_O := as.character(CAUSABAS_O)]
  dt[,CAUSABAS_UNIQUE := data.table::fcase(
    is.na(CAUSABAS_O) & is.na(CAUSABAS),NA_character_,
    is.na(CAUSABAS_O) & !is.na(CAUSABAS),CAUSABAS,
    !is.na(CAUSABAS_O),CAUSABAS_O
  )]
  dt <- dt[grepl("^V",CAUSABAS_UNIQUE) ,]
  
  my_list <- list(
    # V01-V09 Pedestre traumatizado em um acidente de transporte
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v01_v09.htm
    "walk" = sort(c(
      paste0("V0",rep(1:8,2),rep(c(1,9),each = 8))
      ,paste0("V0",92:99)
    )),
    # V10-V19 Ciclista traumatizado em um acidente de transporte
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v10_v19.htm
    "bike" = sort(c(
      paste0("V",rep(10:18,6),rep(4:9,each = 9))
      ,paste0("V",194:199)
    )),
    # V20-V29 Motociclista traumatizado em um acidente de transporte
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v20_v29.htm
    "moto" = sort(c(
      paste0("V",rep(20:28,6),rep(4:9,each = 9))
      ,paste0("V",294:299)
    )),
    # V30-V39 Ocupante de triciclo motorizado traumatizado em um acidente de transporte
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v30_v39.htm
    "tric" = sort(c(
      paste0("V",rep(30:38,5),rep(5:9,each = 9))
      ,paste0("V",394:399)
    )),
    # V40-V49 Ocupante de um automóvel traumatizado em um acidente de transporte
    # V50-V59 Ocupante de uma caminhonete traumatizado em um acidente de transporte
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v40_v49.htm
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v50_v59.htm
    "auto" = sort(c(
      paste0("V",rep(40:48,5),rep(5:9,each = 9))
      ,paste0("V",494:499)
      ,paste0("V",rep(50:58,5),rep(5:9,each = 9))
      ,paste0("V",594:599)
    )),
    # V60-V69 Ocupante de um veículo de transporte pesado traumatizado em um acidente de transporte
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v60_v69.htm
    "cami" = sort(c(
      paste0("V",rep(60:68,5),rep(5:9,each = 9)),
      paste0("V",694:699)
    )),
    # V70-V79 Ocupante de um ônibus traumatizado em um acidente de transporte
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v70_v79.htm
    "onib" = paste0("V",rep(70:79,5),rep(5:9,each = 10)),
    # V80-V89 Outros acidentes de transporte terrestre
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v80_v89.htm
    # http://www2.datasus.gov.br/cid10/V2008/WebHelp/v98_v99.htm
    "othe" = paste0("V",c(80,821,829,830:833,840:843,850:853
                          ,860:863,870:879,892:893,98,99))
  )
  
  for(i in names(my_list)){ # i = "walk"
    vars <- my_list[i][[1]]
    for(j in vars){ # j = vars[1]
      dt[CAUSABAS_UNIQUE == j,causa_name := i]
    }
  }
  
  dt <- dt[!is.na(causa_name),]
  
  # date ext and int
  dt[,date_nasc := as.Date(DTNASC,"%d%m%Y")]
  dt[,date := as.Date(DTOBITO,"%d%m%Y")]
  dt[,year := format(date,"%Y")]
  dt[,age := date - date_nasc]
  dt[,age := round(as.numeric(age)/365,0)]
  dt[,sexo := fcase(SEXO == 1,"Homens",SEXO == 2,"Mulheres")]
  dt[is.na(sexo),sexo := "Sem declaração"]
  dt[,cor := fcase(RACACOR == 1,"Branca"
                   ,RACACOR %in% c(2,4),"Negra"
                   ,RACACOR == 3,"Amarela"
                   ,RACACOR == 5,"Indígena"
                   ,RACACOR %in% c(0,6,7,8,9),"Sem declaração"
  )]
  dt[is.na(cor),cor := "Sem declaração"]
  # fix age
  dt[age >=0 &  age<18, AGE :="0-17"]
  dt[age >17 &  age<25, AGE :="18-24"]
  dt[age >=25 & age<30, AGE :="25-29"]
  dt[age >=30 & age<40, AGE :="30-39"]
  dt[age >=40 & age<50, AGE :="40-49"]
  dt[age >=50 & age<60, AGE :="50-59"]
  dt[age >=60 & age<70, AGE :="60-69"]
  dt[age >=70         , AGE :="70+"]
  dt[is.na(age)       , AGE :="Sem declaração"]
  
  dt <- dt[,list("num_acid" = sum(.N,na.rm = TRUE))
           ,by = .(CODMUNOCOR,causa_name,year,AGE,sexo,cor)]
  setnames(dt,"CODMUNOCOR","code_muni_sus")
  
  # return
  return(dt)
}
## DT_MERGE ------

dt_merge <- rbind(
  filter_datasus("data-raw/datasus/SIM-DOINF_2011_to_2012.rds"),
  filter_datasus("data-raw/datasus/SIM-DOEXT_2011_to_2012.rds"),
  filter_datasus("data-raw/datasus/SIM-DOINF_2013_to_2014.rds"),
  filter_datasus("data-raw/datasus/SIM-DOEXT_2013_to_2014.rds"),
  filter_datasus("data-raw/datasus/SIM-DOINF_2015_to_2019.rds"),
  filter_datasus("data-raw/datasus/SIM-DOEXT_2015_to_2019.rds"),
  filter_datasus("data-raw/datasus/SIM-DOINF_2020_to_2021.rds"),
  filter_datasus("data-raw/datasus/SIM-DOEXT_2020_to_2021.rds")
)

# readr::write_rds(dt_merge,"data/datasus/deaths_cor_age_sexo.rds")

# compare data
dt_merge[,sum(num_acid),by = .(year)]
dt_merge[!is.na(causa_name),sum(num_acid),by = .(year)]
dt_merge[,sum(num_acid),by = .(year,causa_name)][order(year)]
# sum
dt_merge <- dt_merge[,list(total_acid = sum(num_acid,na.rm = TRUE))
                     ,by = .(code_muni_sus
                             ,causa_name,year
                             ,sexo,cor,AGE)]
dt_merge$causa_name %>% table(useNA = "always")
dt_merge$year %>% table(useNA = "always")
dt_merge$cor %>% table(useNA = "always")
dt_merge$sexo %>% table(useNA = "always")
dt_merge$AGE %>% table(useNA = "always")


# 3) Read censo 2010 ---- 

ibge_2010 <- readr::read_rds("data-raw/sidrar/pop_2010_sexo_age_raca.rds")

# fix race
setnames(ibge_2010,"cor_ou_raca","cor")
ibge_2010[cor %in% c("Preta","Parda"),cor := "Negra"]
# fix sexo
ibge_2010[is.na(sexo),sexo := "Sem declaração"]
# fix age
ibge_2010[,idade := grupo_de_idade %>% 
            gsub(" a "," - ",.) %>% 
            gsub(" anos","",.) %>% 
            gsub(" ou mais","+",.)]

# TESTE VALORES 
# # test 1
# ibge_2010[(idade %in% c("15 - 19")),sum(valor,na.rm = TRUE)]
# ibge_2010[(idade %in% c("15 - 17","18 e 19")),sum(valor,na.rm = TRUE)]
# # test 2
# ibge_2010[(idade %in% c("70+")),sum(valor,na.rm = TRUE)]
# ibge_2010[(idade %in% c("70 - 79",'80+')),sum(valor,na.rm = TRUE)]
# # total
ibge_2010[idade %in%  c("Total"),sum(valor,na.rm = TRUE)]

# remove extra intervals of age
sort(unique(ibge_2010$idade))
ibge_2010 <- ibge_2010[!(idade %in% c('0 - 14','15 - 19'
                                      ,'15 - 64','65+'
                                      ,"70 - 79",'80+','Total')),]
ibge_2010[,sum(valor,na.rm = TRUE)] # check [1] 190755723
# create new age intervals
ibge_2010[idade %in% c("0 - 4","5 - 9","10 - 14","15 - 17") , AGE :="0-17"]
ibge_2010[idade %in% c('18 e 19','20 - 24')  , AGE :="18-24"]
ibge_2010[idade %in% c('25 - 29'), AGE :="25-29"]
ibge_2010[idade %in% c('30 - 39'), AGE :="30-39"]
ibge_2010[idade %in% c('40 - 49'), AGE :="40-49"]
ibge_2010[idade %in% c('50 - 59'), AGE :="50-59"]
ibge_2010[idade %in% c('60 - 69'), AGE :="60-69"]
ibge_2010[idade %in% c('70+')    , AGE :="70+"]

# check columns
ibge_2010$year %>% table(useNA = "always")
ibge_2010$sexo %>% table(useNA = "always")
ibge_2010$cor %>% table(useNA = "always")
ibge_2010$AGE %>% table(useNA = "always")

# sum population
ibge_2010 <- ibge_2010[,list(pop = sum(valor,na.rm = TRUE))
                       ,by = .(municipio_codigo,code_muni_sus
                               ,year,cor,sexo,AGE)] 

## Censo w/ metro info ----
dt_metro <- geobr::read_metro_area()
setDT(dt_metro)
dt_metro[,code_muni := as.character(code_muni)]

ibge_2010 <- ibge_2010[dt_metro, on = c('municipio_codigo'= 'code_muni'),
                       name_metro := i.name_metro]
ibge_2010$name_metro %>% table(useNA = "always")
ibge_2010 <- ibge_2010[,list("pop" = sum(pop,na.rm = TRUE))
                       ,by = .(name_metro,AGE,sexo,cor)]
ibge_2010[,prop := pop/sum(pop,na.rm = TRUE),by = name_metro]
ibge_2010[pop == 0,prop := 0]
ibge_2010[,sum(prop),by = name_metro] # should be always 1
ibge_2010[,sum(pop)]                  # 190755723

# 4) Read Pop 2013 - 2019 & Metro Name -----

dt_pop <- readr::read_rds("data-raw/sidrar/pop_2013_to_2019.rds")
## Pop w/ metro info ------
dt_metro <- geobr::read_metro_area()
setDT(dt_metro)
dt_metro[,code_muni_sus := stringr::str_sub(string = code_muni,start = 1,end = 6)]
dt_metro <- dt_metro[,.SD,.SDcols = c("code_muni_sus","name_metro")]

# 5) Merge data -----

## . w/ metro name & pop data ----
tmp_dt_acid <- data.table::merge.data.table(
  x = dt_merge  ,y = dt_metro  ,by = c('code_muni_sus')
) %>% data.table::merge.data.table(
  x = .  ,y = dt_pop  ,by = c('code_muni_sus','year')
)

tmp_dt_acid$causa_name %>% table(useNA = "always")
tmp_dt_acid$year %>% table(useNA = "always")
tmp_dt_acid$cor %>% table(useNA = "always")
tmp_dt_acid$sexo %>% table(useNA = "always")
tmp_dt_acid$AGE %>% table(useNA = "always")
## Rbind .  ----

dados_acid <- rbind(
  # unique
  tmp_dt_acid[,list("deaths" = sum(total_acid,na.rm = TRUE))
              ,by = .(name_metro,causa_name,year,AGE,cor,sexo)],
  # only active
  tmp_dt_acid[causa_name %in% c('walk','bike'),list(
    "deaths" = sum(total_acid,na.rm = TRUE))
    ,by = .(name_metro,year,AGE,cor,sexo)][,causa_name := "walk+bike"],
  # total
  tmp_dt_acid[,list("deaths" = sum(total_acid,na.rm = TRUE))
              ,by = .(name_metro,year,AGE,cor,sexo)][,causa_name := "total"]
)

# check columns
dados_acid$year %>% table(useNA = "always")
dados_acid$sexo %>% table(useNA = "always")
dados_acid$cor %>% table(useNA = "always")
dados_acid$AGE %>% table(useNA = "always")

## . w/ RM pop 2013-2019 ----

tmp_pop_rm <- copy(tmp_dt_acid)[,.SD[1],by = .(name_metro, municipio_codigo,year)]
tmp_pop_rm <- tmp_pop_rm[,list(pop = sum(valor)),by = .(name_metro,year)]
tmp_pop_rm[name_metro == "RM São Paulo"] # check

dados_acid <- dados_acid[tmp_pop_rm,on = c("name_metro","year")]
dados_acid[name_metro == "RM Porto Velho",]
dados_acid[name_metro == "RM São Paulo" & is.na(AGE),]
dados_acid[name_metro == "RM São Paulo" & causa_name == "bike" & sexo == "Homens",]

dados_acid$causa_name %>% table(useNA = "always")
dados_acid$year %>% table(useNA = "always")
dados_acid$cor %>% table(useNA = "always")
dados_acid$sexo %>% table(useNA = "always")
dados_acid$AGE %>% table(useNA = "always")

## . w/ IBGE 2010 prop----
ibge_2010$AGE %>% table(useNA = "always")
ibge_2010$sexo %>% table(useNA = "always")
ibge_2010$AGE %>% table(useNA = "always")
ibge_2010[1]
dados_acid[1]
dados_acid[ibge_2010,on = c("name_metro","AGE","sexo","cor"),prop := i.prop]

# checking
dados_acid[is.na(prop)]
dados_acid[causa_name == "total" & name_metro == "RM Porto Velho",]
dados_acid[causa_name == "total",sum(prop,na.rm = TRUE),
           ,by = .(year,name_metro)] 
dados_acid[causa_name == "total",sum(pop * prop,na.rm = TRUE)
           ,by = .(year,name_metro)] 

# save data ----
readr::write_rds(dados_acid,"data/datasus/metro_by_mode_age_cor_sexo.rds")



