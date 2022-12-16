

# remotes::install_github("rfsaldanha/microdatasus")
rm(list=ls())


easypackages::packages('data.table'
                       ,'magrittr'
                       ,'microdatasus'
                       ,'basedosdados')

# download data
state <- geobr::read_state()
state <- state$abbrev_state

str_acid <- c(paste0("V0",c(1:89,98,99))
              ,paste0("V",c(1:89,98,99)))
str_acid <- c(paste0("V0",c(90:97))
              ,paste0("V",c(90:97)))

# download acid-------
dados_ext <- microdatasus::fetch_datasus(year_start = 2019
                                         , year_end = 2019
                                         , information_system = "SIM-DOEXT")
dados_inf <- microdatasus::fetch_datasus(year_start = 2019
                                         , year_end = 2019
                                         , information_system = "SIM-DOINF")
setDT(dados_ext)
setDT(dados_inf)

# apenas acidentes
dados1_ext <- dados_ext[grepl("^V",CAUSABAS_O),]
dados1_inf <- dados_inf[grepl("^V",CAUSABAS_O),]

# remover acidentes que nao sao de transporte terrestre
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
dados1_ext[CAUSABAS_O %in% code_walk,]
dados_acid <- rbind(
  dados1_ext[,.N,by = .(CODMUNOCOR,causa_name)],
  dados1_inf[,.N,by = .(CODMUNOCOR,causa_name)])

dados_acid <- dados_acid[,list(num_acid = sum(N)),by = .(CODMUNOCOR,causa_name)]
setnames(dados_acid,"CODMUNOCOR","code_muni_sus")

# download pop proj 2019 -------

sidrar::info_sidra(6579)
dt_pop <- sidrar::get_sidra(x = 6579
                            , period = "2019"
                            , variable = 9324
                            , geo = "City")
setDT(dt_pop)
names(dt_pop) <- janitor::make_clean_names(names(dt_pop))
dt_pop[,code_muni_sus := stringr::str_sub(string = municipio_codigo,start = 1,end = 6)]
dt_pop <- dt_pop[,.SD,.SDcols = c("municipio_codigo","code_muni_sus","valor")]
dt_pop[1,]

# metro region ----

dt_metro <- geobr::read_metro_area()
setDT(dt_metro)
dt_metro[,code_muni_sus := stringr::str_sub(string = code_muni,start = 1,end = 6)]
dt_metro <- dt_metro[,.SD,.SDcols = c("code_muni_sus","name_metro")]
dt_metro 


# merge data ----

dados_acid1 <- data.table::merge.data.table(
  x = dados_acid
  ,y = dt_metro
  ,by = 'code_muni_sus'
)
dados_acid1 <- data.table::merge.data.table(
  x = dados_acid1
  ,y = dt_pop
  ,by = 'code_muni_sus'
)

dados_acid1 <- dados_acid1[,list("pop" = sum(valor,na.rm = TRUE)
                                 ,"deaths" = sum(num_acid,na.rm = TRUE))
                           ,by = .(name_metro)]

dados_acid1[,deaths_100k := deaths / (pop / 100000)]

dados_acid1[1:2,]
dt_metro[1:2,]

#dados_acid1[dt_metro,on = c("code_muni" = "code_muni_sus"),name_metro := i.name_metro]

# save data ----

readr::write_rds(dados_acid1,"data/datasus/deaths_roads_metro.rds")



dados_acid <- readr::read_rds("data/datasus/deaths_roads_metro.rds")

dados_acid[1:5,]
