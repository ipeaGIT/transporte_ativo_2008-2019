# download acid-------

#' Wiki do Projeto
#' @site  <<https://github.com/rfsaldanha/microdatasus/wiki>>
#' @site <<https://bookdown.org/labxss/coorte_adm2/introdu%C3%A7%C3%A3o.html#>>
#' @site <<https://github.com/rfsaldanha/microdatasus/wiki/Conven%C3%A7%C3%B5es-SIM>>
#' @legenda
#'
#' SIM	DO	Declarações de Óbitos
#' SIM	DOEXT	Declarações de Óbitos por causas externas
#' SIM	DOINF	Declarações de Óbitos infantis
#' 
#' 
dados_ext <- microdatasus::fetch_datasus(year_start = 2011
                                         , year_end = 2012
                                         , information_system = "SIM-DOEXT")
readr::write_rds(dados_ext,"data-raw/datasus/SIM-DOEXT_2011_to_2012.rds")

dados_ext <- microdatasus::fetch_datasus(year_start = 2013
                                         , year_end = 2014
                                         , information_system = "SIM-DOEXT")
readr::write_rds(dados_ext,"data-raw/datasus/SIM-DOEXT_2013_to_2014.rds")
dados_ext <- microdatasus::fetch_datasus(year_start = 2015
                                         , year_end = 2019
                                         , information_system = "SIM-DOEXT")
readr::write_rds(dados_ext,"data-raw/datasus/SIM-DOEXT_2015_to_2019.rds"
                 ,compress = "gz")
dados_ext <- microdatasus::fetch_datasus(year_start = 2020
                                         , year_end = 2021
                                         , information_system = "SIM-DOEXT")
readr::write_rds(dados_ext,"data-raw/datasus/SIM-DOEXT_2020_to_2021.rds"
                 ,compress = "gz")

dados_inf <- microdatasus::fetch_datasus(year_start = 2011
                                         ,year_end = 2012
                                         ,information_system = "SIM-DOINF")
readr::write_rds(dados_inf,"data-raw/datasus/SIM-DOINF_2011_to_2012.rds"
                 ,compress = "gz")
dados_inf <- microdatasus::fetch_datasus(year_start = 2013
                                         , year_end = 2014
                                         , information_system = "SIM-DOINF")
readr::write_rds(dados_inf,"data-raw/datasus/SIM-DOINF_2013_to_2014.rds"
                 ,compress = "gz")

dados_inf <- microdatasus::fetch_datasus(year_start = 2015
                                         , year_end = 2019
                                         , information_system = "SIM-DOINF")
readr::write_rds(dados_inf,"data-raw/datasus/SIM-DOINF_2015_to_2019.rds"
                 ,compress = "gz")

dados_inf <- microdatasus::fetch_datasus(year_start = 2020
                                         , year_end = 2021
                                         , information_system = "SIM-DOINF")
readr::write_rds(dados_inf,"data-raw/datasus/SIM-DOINF_2020_to_2021.rds"
                 ,compress = "gz")
