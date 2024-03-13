# download acid-------

# create dir
dir.create("data-raw/datasus/", recursive = T)

#' Wiki do Projeto
#' @site  <<https://github.com/rfsaldanha/microdatasus/wiki>>
#' @site <<https://bookdown.org/labxss/coorte_adm2/introdu%C3%A7%C3%A3o.html#>>
#' @site <<https://github.com/rfsaldanha/microdatasus/wiki/Conven%C3%A7%C3%B5es-SIM>>
#' @legenda
#'
#' SIM	DO	Declarações de Óbitos
#' SIM	DOEXT	Declarações de Óbitos por causas externas
#' SIM	DOINF	Declarações de Óbitos infantis


# DOEXT	Declarações de Óbitos por causas externas
dados_ext <- microdatasus::fetch_datasus(year_start = 2011
                                         , year_end = 2022
                                         , information_system = "SIM-DOEXT")
readr::write_rds(dados_ext,"data-raw/datasus/SIM-DOEXT_2011_to_2022.rds"
                 ,compress = "gz")


# clean memory
rm(dados_ext)
gc()

# SIM	DOINF	Declarações de Óbitos infantis
dados_inf <- microdatasus::fetch_datasus(year_start = 2011
                                         ,year_end = 2022
                                         ,information_system = "SIM-DOINF")
readr::write_rds(dados_inf,"data-raw/datasus/SIM-DOINF_2011_to_2022.rds"
                 ,compress = "gz")

