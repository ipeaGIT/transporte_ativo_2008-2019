# Load packages -----
rm(list=ls())

# remotes::install_github("rfsaldanha/microdatasus")
#  devtools::install_github("danicat/read.dbc")
easypackages::packages('data.table'
                       ,'magrittr'
                       ,'ggplot2'
                       ,'microdatasus')




# 1) Read acid ----- 
filter_datasus <- function(filepath){
  
  # filepath <- "../../data/transporte_ativo_2008-2019/export_datasus/SIM-DOEXT_2011_to_2021.rds"
  
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
  
  # causas externas
  dt <- dt[grepl("^V",CAUSABAS_UNIQUE) ,]
  
  # using 2-digit CID-10
  dt[, CAUSABAS_UNIQUE_2d := substring(CAUSABAS_UNIQUE, 1,3)]
  
  # test
  dt[grepl("^V0",CAUSABAS_UNIQUE) ,]$CAUSABAS_UNIQUE_2d %>% unique()
  dt[grepl("^V0",CAUSABAS_UNIQUE) ,]$CAUSABAS_UNIQUE %>% unique()
  my_list <- list(
    #' @V01-V09 Pedestre traumatizado em um acidente de transporte
    #' <<http://www2.datasus.gov.br/cid10/V2008/WebHelp/v01_v09.htm>>
    "walk" = sort(c(
      paste0("V0",(0:9))
    )),
    #' @V10-V19 Ciclista traumatizado em um acidente de transporte
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v10_v19.htm
    "bike" = sort(c(
      paste0("V",(10:19))
    )),
    #' @V20-V29 Motociclista traumatizado em um acidente de transporte
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v20_v29.htm
    "moto" = sort(c(
      paste0("V",(20:29))
    )),
    #' @V30-V39 Ocupante de triciclo motorizado traumatizado em um acidente de transporte
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v30_v39.htm
    "tric" = sort(c(
      paste0("V",(30:39))
    )),
    #' @V40-V49 Ocupante de um automóvel traumatizado em um acidente de transporte
    #' @V50-V59 Ocupante de uma caminhonete traumatizado em um acidente de transporte
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v40_v49.htm
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v50_v59.htm
    "auto" = sort(c(
      paste0("V",(40:49))
      ,paste0("V",(50:59))
    )),
    #' @V60-V69 Ocupante de um veículo de transporte pesado traumatizado em um acidente de transporte
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v60_v69.htm
    "cami" = sort(c(
      paste0("V",(60:69))
    )),
    #' @V70-V79 Ocupante de um ônibus traumatizado em um acidente de transporte
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v70_v79.htm
    "onib" = paste0("V",(70:79)),
    #' @V80-V89 Outros acidentes de transporte terrestre
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v80_v89.htm
    #' http://www2.datasus.gov.br/cid10/V2008/WebHelp/v98_v99.htm
    "othe" = paste0("V",(80:89))
  )
  
  dt[, causa_name := fcase(CAUSABAS_UNIQUE_2d %in% my_list$walk, 'walk',
                           CAUSABAS_UNIQUE_2d %in% my_list$bike, 'bike',
                           CAUSABAS_UNIQUE_2d %in% my_list$moto, 'moto',
                           CAUSABAS_UNIQUE_2d %in% my_list$tric, 'tric',
                           CAUSABAS_UNIQUE_2d %in% my_list$auto, 'auto',
                           CAUSABAS_UNIQUE_2d %in% my_list$cami, 'cami',
                           CAUSABAS_UNIQUE_2d %in% my_list$onib, 'onib',
                           CAUSABAS_UNIQUE_2d %in% my_list$othe, 'othe',
                           CAUSABAS_UNIQUE_2d %in% 'V99',        'undetermined',
                           default = NA
  )]
  
  # acidentes nao de transito
  not_transport <- c( paste0("V0",10* 1:9), 'V091'
                      
                      , paste0("V10", 0:2)
                      , paste0("V11", 0:2)
                      , paste0("V12", 0:2)
                      , paste0("V13", 0:2)
                      , paste0("V14", 0:2)
                      , paste0("V15", 0:2)
                      , paste0("V16", 0:2)
                      , paste0("V17", 0:2)
                      , paste0("V18", 0:2)
                      
                      , paste0("V20", 0:2)
                      , paste0("V21", 0:2)
                      , paste0("V22", 0:2)
                      , paste0("V23", 0:2)
                      , paste0("V24", 0:2)
                      , paste0("V25", 0:2)
                      , paste0("V26", 0:2)
                      , paste0("V27", 0:2)
                      , paste0("V28", 0:2)
                      
                      , paste0("V30", 0:2)
                      , paste0("V31", 0:2)
                      , paste0("V32", 0:2)
                      , paste0("V33", 0:2)
                      , paste0("V34", 0:2)
                      , paste0("V35", 0:2)
                      , paste0("V36", 0:2)
                      , paste0("V37", 0:2)
                      , paste0("V38", 0:2)
                      
                      , paste0("V50", 0:2)
                      , paste0("V51", 0:2)
                      , paste0("V52", 0:2)
                      , paste0("V53", 0:2)
                      , paste0("V54", 0:2)
                      , paste0("V55", 0:2)
                      , paste0("V56", 0:2)
                      , paste0("V57", 0:2)
                      , paste0("V58", 0:2)
                      
                      , paste0("V60", 0:2)
                      , paste0("V61", 0:2)
                      , paste0("V62", 0:2)
                      , paste0("V63", 0:2)
                      , paste0("V64", 0:2)
                      , paste0("V65", 0:2)
                      , paste0("V66", 0:2)
                      , paste0("V67", 0:2)
                      , paste0("V68", 0:2)
                      
                      , paste0("V70", 0:2)
                      , paste0("V71", 0:2)
                      , paste0("V72", 0:2)
                      , paste0("V73", 0:2)
                      , paste0("V74", 0:2)
                      , paste0("V75", 0:2)
                      , paste0("V76", 0:2)
                      , paste0("V77", 0:2)
                      , paste0("V78", 0:2)
                      
                      , paste0("V80", 0:2)
                      , paste0("V81", 0:2)
                      , paste0("V82", 0:2)
                      , paste0("V83", 0:2)
                      , paste0("V84", 0:2)
                      , paste0("V85", 0:2)
                      , paste0("V86", 0:2)
                      , paste0("V87", 0:2)
                      , paste0("V88", 0:2)
                      
                      
                      , paste0("V19", 0:3)
                      , paste0("V29", 0:3)
                      , paste0("V39", 0:3)
                      , paste0("V49", 0:3)
                      , paste0("V59", 0:3)
                      , paste0("V69", 0:3)
                      , paste0("V79", 0:3)
                      , paste0("V89", 0:3)
  )
  
  dt[ CAUSABAS_UNIQUE %in% not_transport, causa_name := NA]
  table(dt$causa_name, useNA = 'always')
  
  
  # drop NAs
  dt <- dt[!is.na(causa_name),]
  
  # date ext and int
  dt[,time := as.numeric(HORAOBITO)]
  dt[,time := as.numeric(substring(time, 1, 2))]
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
  dt[, AGE := fcase(age >=0 &  age<18, "0-17",
                    age >17 &  age<25, "18-24",
                    age >=25 & age<30, "25-29",
                    age >=30 & age<40, "30-39",
                    age >=40 & age<50, "40-49",
                    age >=50 & age<60, "50-59",
                    age >=60 & age<70, "60-69",
                    age >=70         , "70+",
                    is.na(age)       , "Sem declaração")]
  
  
  
  # # time of the day-----
  # head(dt$HORAOBITO)
  # dt[,time := as.numeric(HORAOBITO)]
  # dt[,time := substring(time, 1, 2)]
  # dt[,time := as.numeric(time)]
  # 
  # summary(dt$time)
  # dt[is.na(time), .N] / nrow(dt)
  # 
  # dt2 <- dt[ time <=24]
  # nrow(dt2) / nrow(dt)
  # 
  # 
  # dt2[, period := ifelse(time > 7 & time < 18, 'day', 'night') ]
  # 
  # 
  # 
  # dt2 <- dt2[, .(deaths = .N), by = .(year, period, causa_name)]
  # 
  # dt2[, groups := paste0(period, causa_name)]
  # 
  # ggplot(data = dt2) +
  #   geom_line( aes(x=year, y=deaths, color=period, group=groups)) +
  #   facet_wrap(.~causa_name,ncol  = 3,
  #              scales = "free_y")
  # 
  # # time of the day
  
  # check missing values
  table(dt$sex, useNA = 'always') / nrow(dt) *100
  table(dt$AGE, useNA = 'always') / nrow(dt) *100
  table(dt$cor, useNA = 'always') / nrow(dt) *100
  table(dt$causa_name, useNA = 'always') / nrow(dt) *100
  
  # summarize
  dt2 <- dt[,list("num_acid" = .N)
            ,by = .(CODMUNOCOR,causa_name,year,AGE,sexo,cor)]
  
  setnames(dt2,"CODMUNOCOR","code_muni_sus")
  
  # return
  return(dt2)
}



## Get micro data clean ------

filepath <-  "../../data/transporte_ativo_2008-2019/export_datasus/SIM-DOEXT_2011_to_2021.rds"


dt_merge <- filter_datasus(filepath)

# check capitals
330455 # rj
355030 # SP
310620 # bh
410690 # cwb
dt_merge[causa_name == "bike" & code_muni_sus == 330455,sum(num_acid),by = .(year)]
dt_merge[causa_name == "bike" & code_muni_sus == 355030,sum(num_acid),by = .(year)]
dt_merge[causa_name == "bike" & code_muni_sus == 310620,sum(num_acid),by = .(year)]
dt_merge[causa_name == "bike" & code_muni_sus == 410690,sum(num_acid),by = .(year)]



readr::write_rds(dt_merge,"data/datasus/deaths_cor_age_sexo.rds"
                 ,compress = "gz")

# 2) Read data and merge with metropolitan area and population -------
rm(list=ls())

dt_merge <- readr::read_rds("data/datasus/deaths_cor_age_sexo.rds")

# check capitals
 # rj 330455
dt_merge[causa_name == "bike" & code_muni_sus == 330455,sum(num_acid),by = .(year)]
 # SP355030
dt_merge[causa_name == "bike" & code_muni_sus == 355030,sum(num_acid),by = .(year)]
# bh 310620 
dt_merge[causa_name == "bike" & code_muni_sus == 310620,sum(num_acid),by = .(year)]
# cwb 410690 
dt_merge[causa_name == "bike" & code_muni_sus == 410690,sum(num_acid),by = .(year)]

# compare data
dt_merge[,sum(num_acid),by = .(year)] |> plot()
dt_merge[causa_name == 'bike',sum(num_acid),by = .(year)] 
dt_merge[causa_name == 'walk',sum(num_acid),by = .(year)] 
dt_merge[causa_name == 'moto',sum(num_acid),by = .(year)] 
dt_merge[causa_name == 'auto',sum(num_acid),by = .(year)] 
dt_merge[!is.na(causa_name),sum(num_acid),by = .(year)]
dt_merge[,sum(num_acid),by = .(year,causa_name)][order(year)]

# sum by code_muni_sus
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
head(ibge_2010)
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
ibge_2010$idade %>% table(useNA = "always")
# # test 1
# ibge_2010[(idade %in% c("15 - 19")),sum(valor,na.rm = TRUE)]
# ibge_2010[(idade %in% c("15 - 17","18 e 19")),sum(valor,na.rm = TRUE)]
# # test 2
# ibge_2010[(idade %in% c("70+")),sum(valor,na.rm = TRUE)]
# ibge_2010[(idade %in% c("70 - 79",'80+')),sum(valor,na.rm = TRUE)]
# # total
ibge_2010[idade %in%  c("Total"),sum(valor,na.rm = TRUE)]
# remove extra intervals of age+
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
# aggregate population by 
# municipio_codigo,code_muni_sus
# ,year,cor,sexo,AGE
ibge_2010 <- ibge_2010[,list(pop = sum(valor,na.rm = TRUE))
                       ,by = .(municipio_codigo,code_muni_sus
                               ,year,cor,sexo,AGE)]
# ## Censo w/ metro info ----
#dt_metro <- geobr::read_metro_area(year = 2018)
dt_metro <- readr::read_rds("data-raw/sidrar/read_metro_area.rds")
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
readr::write_rds(ibge_2010,file = "data-raw/sidrar/censo_2010_RM.rds")
# # 4) Pop projection & Metro Name -----
#
dt_pop <- readr::read_rds("data-raw/sidrar/pop_2011_to_2021.rds")

dt_pop$year %>% table(useNA = "always")

## Pop w/ metro info ------
dt_metro <- geobr::read_metro_area(year = 2018)
setDT(dt_metro)
dt_metro[,code_muni_sus := stringr::str_sub(string = code_muni,start = 1,end = 6)]
dt_metro <- dt_metro[,.SD,.SDcols = c("code_muni_sus","name_metro")]


# 5) Merge data -----

## . w/ metro name & pop data ----
tmp_dt_acid <- data.table::merge.data.table(
  x = dt_merge,
  y = dt_metro,
  by = c('code_muni_sus'))  %>%
  data.table::merge.data.table(
    x = .  ,y = dt_pop  ,by = c('code_muni_sus','year')
  )

tmp_dt_acid$causa_name %>% table(useNA = "always")
tmp_dt_acid$year %>% table(useNA = "always")
tmp_dt_acid$cor %>% table(useNA = "always")
tmp_dt_acid$sexo %>% table(useNA = "always")
tmp_dt_acid$AGE %>% table(useNA = "always")

# ## Rbind .  ----

#dados_acid <- tmp_dt_acid[, list("deaths" = sum(total_acid,na.rm = TRUE))
#                          ,by = .(name_metro,causa_name,year,AGE,cor,sexo)]



dados_acid <- rbind(
  # unique
  tmp_dt_acid[,list("deaths" = sum(total_acid,na.rm = TRUE))
              ,by = .(name_metro,causa_name,year,AGE,cor,sexo)] ,
  # only active
  tmp_dt_acid[causa_name %in% c('walk','bike'),list(
    "deaths" = sum(total_acid,na.rm = TRUE))
    ,by = .(name_metro,year,AGE,cor,sexo)][,causa_name := "walk+bike"],
  # total
  tmp_dt_acid[,list("deaths" = sum(total_acid,na.rm = TRUE))
              ,by = .(name_metro,year,AGE,cor,sexo)][,causa_name := "total"]
)

# check columns
tmp_dt_acid$year %>% table(useNA = "always")
tmp_dt_acid$sexo %>% table(useNA = "always")
tmp_dt_acid$cor %>% table(useNA = "always")
tmp_dt_acid$AGE %>% table(useNA = "always")



## . w/ RM pop 2011-2021 ----
tmp_pop_rm <- copy(tmp_dt_acid)[,.SD[1],by = .(valor,name_metro, municipio_codigo,year)] %>% 
  .[,.SD,.SDcols = c('valor','name_metro', 'municipio_codigo','year')]
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
readr::write_rds(dados_acid, "./data/datasus/metro_by_mode_age_cor_sexo.rds"
                 , compress = 'gz')



