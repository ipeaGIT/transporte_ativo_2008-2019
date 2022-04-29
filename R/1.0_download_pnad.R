## download PNAD-----
#devtools::install_github("Gabriel-Assuncao/PNADcIBGE")
# https://www.ibge.gov.br/estatisticas/sociais/educacao/9127-pesquisa-nacional-por-amostra-de-domicilios.html?edicao=18338&t=microdados


library(PNADcIBGE)
library(survey)
library(srvyr)
# devtools::install_github("lucasmation/microdadosBrasil")
library(microdadosBrasil)
# # Downloading data
# source("R/0.1_parse_sasci.R") #


txt_file <- "data-raw/PNAD/PNAD_reponderado_2008/2008/dados/PES2008.TXT"

pes <- microdadosBrasil::read_PNAD(ft = "pessoas"
                                   ,i = 2008
                                   ,file = txt_file)


pnadc.df <- PNADcIBGE::
pnadc.df2 <- PNADcIBGE::get_pnadc(year=2020,
                                  quarter=4,
                                  vars="V1022",
                                  defyear=2020,
                                  defperiod=4,
                                  labels=TRUE,
                                  deflator=TRUE,
                                  design=FALSE,
                                  savedir=tempdir())

pnadc.svy2 <- pnadc_design(data_pnadc=pnadc.df2)
# Calculating unemployment rate
pnadc.svy2_rate <- survey::svymean(x=~V1022, design=pnadc.svy2, na.rm=TRUE)

pnadc.svy2_dt <- data.table::as.data.table(pnadc.svy2_rate)
pnadc.svy2_dt[,V1022 := c(1:2)]
pnadc.svy2_dt[,SE :=NULL]

# save PNAD----
readr::write_rds(pnadc.svy2_dt,"data/pnadc.rds",compress = "gz")