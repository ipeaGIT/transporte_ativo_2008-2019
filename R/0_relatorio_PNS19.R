rm(list=ls())
library(PNSIBGE)
library(data.table)
library(magrittr)

# Read the .txt file

pns2019 <- readr::read_rds("../../data-raw/PNS/2019/pns2019.rds")
pns2019_design <- PNSIBGE::pns_design(pns2019)
data.table::setDT(pns2019)
# 
pns2019[,.N,by = .(V0001)]
pns2019[, V0001 := as.numeric(V0001)]
pns2019[V0001 == 11, ":="(uf = "RO",uf_name = "Rondonia") ]
pns2019[V0001 == 12, ":="(uf = "AC",uf_name = "Acre") ]
pns2019[V0001 == 13, ":="(uf = "AM",uf_name = "Amazonas") ]
pns2019[V0001 == 14, ":="(uf = "RR",uf_name = "Roraima") ]
pns2019[V0001 == 15, ":="(uf = "PA",uf_name = "Para") ]
pns2019[V0001 == 16, ":="(uf = "AP",uf_name = "Amapa") ]
pns2019[V0001 == 17, ":="(uf = "TO",uf_name = "Tocantins") ]
pns2019[V0001 == 21, ":="(uf = "MA",uf_name = "Maranhao") ]
pns2019[V0001 == 22, ":="(uf = "PI",uf_name = "Piaui") ]
pns2019[V0001 == 23, ":="(uf = "CE",uf_name = "Ceara") ]
pns2019[V0001 == 24, ":="(uf = "RN",uf_name = "Rio Grande do Norte")]
pns2019[V0001 == 25, ":="(uf = "PB",uf_name = "Paraiba") ]
pns2019[V0001 == 26, ":="(uf = "PE",uf_name = "Pernambuco") ]
pns2019[V0001 == 27, ":="(uf = "AL",uf_name = "Alagoas") ]
pns2019[V0001 == 28, ":="(uf = "AL",uf_name = "Sergipe") ]
pns2019[V0001 == 29, ":="(uf = "BA",uf_name = "Bahia") ]
pns2019[V0001 == 31, ":="(uf = "MG",uf_name = "Minas Gerais") ]
pns2019[V0001 == 32, ":="(uf = "ES",uf_name = "Espirito Santo") ]
pns2019[V0001 == 33, ":="(uf = "RJ",uf_name = "Rio de Janeiro") ]
pns2019[V0001 == 35, ":="(uf = "SP",uf_name = "São Paulo") ]
pns2019[V0001 == 41, ":="(uf = "PR",uf_name = "Parana") ]
pns2019[V0001 == 42, ":="(uf = "SC",uf_name = "Santa Catarina") ]
pns2019[V0001 == 43, ":="(uf = "RS",uf_name = "Rio Grande do Sul") ]
pns2019[V0001 == 50, ":="(uf = "MS",uf_name = "Mato Grosso do Sul") ]
pns2019[V0001 == 51, ":="(uf = "MT",uf_name = "Mato Grosso") ]
pns2019[V0001 == 52, ":="(uf = "GO",uf_name = "Goias") ]
pns2019[V0001 == 53, ":="(uf = "DF",uf_name = "Federal District") ]



# Tamanhos planejado ----
# Tabela 1 - pag. 16 pdf
# Tamanhos planejado e selecionado da amostra para a Pesquisa
# Nacional de Saúde, segundo as Unidades da Federação - 2019

pns2019[,uniqueN(UPA_PNS),by = .(uf)]

pns2019[,.N,by = .(uf)]


# Numero de domicilios----
# Tab.5 - p. 20 - coluna C2

pns2019[,uniqueN(ID_DOMICILIO),by = .(uf)]

# Densidade de moradores por regiao----
# pg. 26 
pns2019[,V0001 := as.numeric(V0001)]
pns2019[V0001 < 20, region :="Norte"]
pns2019[V0001 > 19 & V0001 < 30, region :="Nordeste"]
pns2019[V0001 > 29 & V0001 < 40, region :="Sudeste"]
pns2019[V0001 > 39 & V0001 < 50, region :="Sul"]
pns2019[V0001 > 49 & V0001 < 60, region :="Centro-Oeste"]

# exemplo
pns2019[ID_DOMICILIO == "1100000160001"
        ,.SD
        ,.SDcols = c("ID_DOMICILIO","V0022","C008")] 

pns2019[,id_V0022 := 1:.N,by = "ID_DOMICILIO"]
pns2019[id_V0022 != 1,id_V0022 := NA]
pns2019[id_V0022 == 1,id_V0022 := V0022]


pns2019[,sum(id_V0022,na.rm = TRUE) / uniqueN(ID_DOMICILIO)
        ,by = .(region)]
#   region       V1           mean
#1:        Norte 3.345693     3.3
#2:     Nordeste 3.045738     3.0
#3:      Sudeste 2.738541     2.8
#4:          Sul 2.681834     2.7
#5: Centro-Oeste 2.841435     2.9


# brazil-----
# pdf p26 relatorio | 2.9 moradores
# analise | 2.968 moradores
pns2019[,{
  num_dom <- data.table::uniqueN(ID_DOMICILIO)
  rate <- sum(id_V0022,na.rm = TRUE) / num_dom
  list(sum(id_V0022,na.rm = TRUE),num_dom,rate)
}]

# Densidade de moradores regiao------
# fonte     | sudeste  | sul      |  norte   | nordeste | sudeste  |
# relatorio |   2.8    | 2.7      |   3.3    |   3.0    |   2.8    |
# analise   |  2.74    | 2.681834 | 3.345693 | 3.045738 | 2.738541 |
pns2019[,{
  num_dom <- data.table::uniqueN(ID_DOMICILIO)
  rate <- sum(id_V0022,na.rm = TRUE) / num_dom
  list(sum(id_V0022,na.rm = TRUE),num_dom,rate)
}
,by = .(region)]

# Proporção de domicílios particulares permanentes, 
#    por Grandes Regiões, segundo algumas características
#
pns2019[,{
  num_banheiro <- A01401
  
  list(sum(id_V0022,na.rm = TRUE),num_dom,rate)
}
,by = .(region)]

# Cobertura de Plano de Saúde ----
#
# Regiao       |  our | PNS  | 
# Norte        | 11.5 | 13.0 |
# Nordeste     | 14.1 | 14.1 |
# Sudeste      | 34.0 | 34.9 |
# Sul          | 31.3 | 30.5 |
# Centro-Oeste | 27.7 | 26.4 |
#
pns2019[,{
  sim <- sum(I00102 == "1")
  diff_sim <- sum(I00102 != "1")
  total <-  length(I00102)
  list("rate" = round(100 * sim / total,1))
}
,by = .(region)]

pns2019[,{
  sim <- sum(I00102 == "1")
  diff_sim <- sum(I00102 != "1")
  total <-  sum(!is.na(I00102))
  total <-  length(I00102)
  list("rate" = round(100 * sim / total,1))
}
,by = .(uf_name)]



pns2019[,{
  bom <- length(which(I006 == "1")) +  length(which(I006 == "2"))
  total <- length(!is.na(I006))
  list("rate" = round(100 * bom / total,1))
},by = .(C006)]




