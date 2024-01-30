# load ----
rm(list=ls())

easypackages::packages('data.table'
                       ,'magrittr'
                       ,'ggplot2','sidrar'
                       ,'microdatasus')

# pop proj 2019 -------

pop_11_to_21 <- "data-raw/sidrar/pop_2011_to_2021.rds"


if(!file.exists(pop_11_to_21)){
  # 2011 - 2015
  dt_pop0 <- sidrar::get_sidra(x = 6579
                              , period = as.character(2011:2015)
                              , variable = 9324
                              , geo = "City")
  data.table::setDT(dt_pop0)
  # 2016 - 2021
  dt_pop1 <- sidrar::get_sidra(x = 6579
                              , period = as.character(2016:2021)
                              , variable = 9324
                              , geo = "City")
  data.table::setDT(dt_pop1)
  # merge
  dt_pop <- rbind(dt_pop0,dt_pop1)
  #
  names(dt_pop) <- janitor::make_clean_names(names(dt_pop))
  dt_pop[,code_muni_sus := stringr::str_sub(string = municipio_codigo,start = 1,end = 6)]
  setnames(dt_pop,"ano","year")
  dt_pop <- dt_pop[,.SD,.SDcols = c("municipio_codigo","code_muni_sus","valor","year")]
  # save
  readr::write_rds(dt_pop,pop_11_to_21,compress = "gz")
}
# pop 2010 censo ----
sidrar::info_sidra(x = 2093)


VecLoop <- c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25
             , 26, 27, 28, 29, 31, 32, 33, 35, 41, 42
             , 43, 50, 51, 52, 53)
Raca_loop <- c(2776,2777,2778,2779,2780,2781)
dt_pop <- lapply(VecLoop,function(i){ # i = 11
  dt_pop1 <- lapply(Raca_loop,function(j){ #j = 1
    message(sprintf('State %s, Race %s',i,j))
    pop2010 <- sidrar::get_sidra(x = 2093
                                 , period = as.character(2010)
                                 , variable = 93
                                 , geo = "City"
                                 , classific = c("c86","c2","c1","c58")
                                 , category = list(j       # cor ou raca
                                                   ,c(4,5) # homem/mulher
                                                   ,c(0)   #  Situação do domicílio
                                                   ,c(0,100402  # Grupo de idade
                                                      ,1140,1141,1142,6797
                                                      ,1143,2792,92982,1144,1145 
                                                      ,3299,3300,3301,3520 
                                                      ,6798,3244,95252,2503))
                                 , geo.filter = list("State" = i)
    )
    return(data.table::as.data.table(pop2010))
  }) %>% data.table::rbindlist()
  return(dt_pop1)
}) %>% data.table::rbindlist()
data.table::setDT(dt_pop)
names(dt_pop) <- janitor::make_clean_names(names(dt_pop))
dt_pop[,code_muni_sus := stringr::str_sub(string = municipio_codigo,start = 1,end = 6)]
setnames(dt_pop,"ano","year")
# save
pop_2010 <- "data-raw/sidrar/pop_2010_sexo_age_raca.rds"
readr::write_rds(dt_pop,pop_2010,compress = "gz")

# end -----