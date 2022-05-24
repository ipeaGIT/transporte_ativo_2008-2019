# Load libraries -----

# https://www.ibge.gov.br/estatisticas/sociais/educacao/9127-pesquisa-nacional-por-amostra-de-domicilios.html?edicao=18338&t=microdados
rm(list=ls())
gc(reset=TRUE)
library(microdadosBrasil) # devtools::install_github("lucasmation/microdadosBrasil")
library(data.table)
library(magrittr)
library(SAScii)

# Read data ----

# microdados
file_pes_path <- "../../data-raw/PNAD/2008/dados/PES2008.TXT"
file_dom_path <- "../../data-raw/PNAD/2008/dados/DOM2008.txt"

# dicionario
dic_pes_path <- "../../data-raw/PNAD/2008/input/input PES2008.txt"
dic_dom_path <- "../../data-raw/PNAD/2008/input/input DOM2008.txt"


pes_raw <- microdadosBrasil::read_PNAD(ft = "pessoas"
                                       ,i = 2008
                                       ,file = file_pes_path)

dom_raw <- microdadosBrasil::read_PNAD(ft = "domicilios"
                                       ,i = 2008
                                       ,file = file_dom_path)

# Indicate which columns will be read

# Columns data----
mycolsPES <- c( "V0101", # Ano
                "UF",    # State
                "V0102", # Numero de controle
                "V0103", # Numero de serie
                "V0403", # Número da família
                "V4729", # Person Weight
                "V4732", # Peso da familia [Igual ao peso da pessoa de referancia da familia]
                "V8005", # idade na data de referencia
                "V0302", # Sex
                "V0404", # cor ou raca
                "V4803", # Years of schooling
                "V4745", # Nivel de instrucao mais elevado alcancado (todas as pessoas)
                "V4741", # Numeo de componentes familia - 01 a 30 pessoas
                "V4727", # Codigo de area censitaria
                "V9005", # Number of jobs
                "V4805", # Condicao de ocupacao na semana de referencia para
                # pessoas de 10 anos ou mais de idade
                "V4706", # Posição na ocupacao no trabalho principal da semana
                # de referencia para pessoas de 10 anos ou mais de idade
                "V4810", # Job sectos
                "V4721", # Rendimento mensal familiar para todas as unidades 
                # domiciliares (exclusive o rendimento das pessoas cuja condição 
                # na família era pensionista, empregado doméstico ou parente do 
                # empregado doméstico e pessoas de menos de 10 anos de idade) [9999]
                
                # 9999 [ADD JOAO]
                "V4742", # Rendimento mensal domiciliar per capita 
                "V4720", # Rendimento mensal de todas as fontes para pessoas de 
                # 10 anos ou mais de idade
                # --- #
                "V9054", # Tipo de estabelecimento ou onde era exercido o 
                # trabalho principal da semana de referencia
                "V9055", # Work at home
                "V9056", # Ia direto do domicilio em que morava para o trabalho
                "V9057", # Tempo de percurso diario de ida da residencia para o local de trabalho
                "V1409", # walking difficulties due to health condition
                "V14091",# shoping difficulties due to health condition
                "V1410", # Costuma ir a pe ou de bicicleta de casa para o trabalho
                "V1411", # Tempo gasto para ir e voltar do trabalho
                "V2801") # Smoker

# household data
mycolsDOM <- c("V0101", # year
               "UF",    # State
               "V0102", # Numero de controle
               "V0103", # Numero de serie
               "V0105", # Total de moradores
               "V0106", # Total de moradores + 10 de anos
               #"V0403", # Family number 99999 not found 
               #"V4729", # Person Weight 99999 not found
               #"V4732", # Family Weight 99999 not found
               "V4611", # Peso do domicilio
               "V4105", # Codigo de situacao censitaria
               "V2032", # Tem carro ou motocicleta de uso pessoal
               "V4602", # Estrato
               "V4617", # STRAT - Identificação de estrato de município auto-representativo e não auto-representativo
               "V4609", # Population Projection
               "UPA",   # Delimitacao do municipio
               "V4618", # PSU - Unidade primaria de amostragem
               "V4619", # Fator de subamostragem
               "V4610", # Inverso da fracao
               "V0233", # Household registered in 'saude da familia' program
               "V0234") # time since registration

# Filter selected Variables
tmp_pes <- data.table::copy(pes_raw)
data.table::setDT(tmp_pes)
tmp_pes <- tmp_pes[,.SD,.SDcols = mycolsPES]


tmp_dom <- data.table::copy(dom_raw)
data.table::setDT(tmp_dom)
tmp_dom <- tmp_dom[,.SD,.SDcols = mycolsDOM]


# Join IndiViduals and Household data sets
names(tmp_pes)
names(tmp_dom)
table(tmp_pes$V0102)
#    V0101    V0102 V0103 UF
#1:   2008 11000015   001 11
# tmp_dom[V0101 == "2008" & V0102 == "11000015" & V0103 == "001" & UF == "11"] %>% str()
# tmp_pes[V0101 == "2008" & V0102 == "11000015" & V0103 == "001" & UF == "11"] %>% str()

pnad2008_raw <- data.table::merge.data.table(x = tmp_pes
                                         ,y = tmp_dom
                                         , by = c("V0101", "V0102", "V0103","UF")
                                         , all = FALSE)

# Clean memory
rm(list=setdiff(ls(), c("pnad2008_raw")))
gc(reset = T)
pnad2008 <- data.table::copy(pnad2008_raw)
colnames(pnad2008) <- janitor::make_clean_names(colnames(pnad2008))



# Add VARiables ---------------------


## Region ------

# Recode State variable, make it compatible with PNAD
pnad2008[uf == 11, uf_name :="Rondonia"]
pnad2008[uf == 12, uf_name :="Acre"]
pnad2008[uf == 13, uf_name :="Amazonas"]
pnad2008[uf == 14, uf_name :="Roraima"]
pnad2008[uf == 15, uf_name :="Para"]
pnad2008[uf == 16, uf_name :="Amapa"]
pnad2008[uf == 17, uf_name :="Tocantins"]
pnad2008[uf == 21, uf_name :="Maranhao"]
pnad2008[uf == 22, uf_name :="Piaui"]
pnad2008[uf == 23, uf_name :="Ceara"]
pnad2008[uf == 24, uf_name :="Rio Grande do Norte"]
pnad2008[uf == 25, uf_name :="Paraiba"]
pnad2008[uf == 26, uf_name :="Pernambuco"]
pnad2008[uf == 27, uf_name :="Alagoas"]
pnad2008[uf == 28, uf_name :="Sergipe"]
pnad2008[uf == 29, uf_name :="Bahia"]
pnad2008[uf == 31, uf_name :="Minas Gerais"]
pnad2008[uf == 32, uf_name :="Espirito Santo"]
pnad2008[uf == 33, uf_name :="Rio de Janeiro"]
pnad2008[uf == 35, uf_name :="São Paulo"]
pnad2008[uf == 41, uf_name :="Parana"]
pnad2008[uf == 42, uf_name :="Santa Catarina"]
pnad2008[uf == 43, uf_name :="Rio Grande do Sul"]
pnad2008[uf == 50, uf_name :="Mato Grosso do Sul"]
pnad2008[uf == 51, uf_name :="Mato Grosso"]
pnad2008[uf == 52, uf_name :="Goias"]
pnad2008[uf == 53, uf_name :="Federal District"]
table(pnad2008$uf_name)

# Create variable Metropolitan area
pnad2008[v4727 == 1, metro := "Restante das UF"]
pnad2008[uf == 15 & v4727 == 1, metro := "Belém"]
pnad2008[uf == 23 & v4727 == 1, metro := "Fortaleza"]
pnad2008[uf == 26 & v4727 == 1, metro := "Recife"]
pnad2008[uf == 29 & v4727 == 1, metro := "Salvador"]
pnad2008[uf == 31 & v4727 == 1, metro := "Belo Horizonte"]
pnad2008[uf == 33 & v4727 == 1, metro := "Rio de Janeiro"]
pnad2008[uf == 35 & v4727 == 1, metro := "São Paulo"]
pnad2008[uf == 41 & v4727 == 1, metro := "Curitiba"]
pnad2008[uf == 43 & v4727 == 1, metro := "Porto Alegre"]
pnad2008[uf == 53 & v4727 == 1, metro := "Distrito Federal"]
table(pnad2008$metro,exclude=FALSE)

pnad2008[,country := "Brasil"]
pnad2008[,dummyMetro := ifelse(is.na(metro),"Non-metro","Metro")]
table(pnad2008$dummyMetro,exclude=FALSE)
gc(reset = T)

# Recode Urban vs Rural variable
pnad2008[v4105<4, urban := "Urbano"]
pnad2008[v4105>3 & v4105<9, urban := "Rural"]
table(pnad2008$urban,exclude = FALSE)


# year variable     
pnad2008[, year := 2008]

# # Count variable     
# pnad2008[, vcount := 1]
# table(pnad2008$vcount)

# Create Great Geographical Regions [data.table]
pnad2008[uf < 20, region :="Norte"]
pnad2008[uf > 19 & uf < 30, region :="Nordeste"]
pnad2008[uf > 29 & uf < 40, region :="Sudeste"]
pnad2008[uf > 39 & uf < 50, region :="Sul"]
pnad2008[uf > 49 & uf < 60, region :="Centro-Oeste"]
table(pnad2008$region)

# Recode State variable, make it compatible with PNAD
pnad2008[uf == 11, uf := "RO"]
pnad2008[uf == 12, uf := "AC"]
pnad2008[uf == 13, uf := "AM"]
pnad2008[uf == 14, uf := "RR"]
pnad2008[uf == 15, uf := "PA"]
pnad2008[uf == 16, uf := "AP"]
pnad2008[uf == 17, uf := "TO"]
pnad2008[uf == 21, uf := "MA"]
pnad2008[uf == 22, uf := "PI"]
pnad2008[uf == 23, uf := "CE"]
pnad2008[uf == 24, uf := "RN"]
pnad2008[uf == 25, uf := "PB"]
pnad2008[uf == 26, uf := "PE"]
pnad2008[uf == 27, uf := "AL"]
pnad2008[uf == 28, uf := "AL"]
pnad2008[uf == 29, uf := "BA"]
pnad2008[uf == 31, uf := "MG"]
pnad2008[uf == 32, uf := "ES"]
pnad2008[uf == 33, uf := "RJ"]
pnad2008[uf == 35, uf := "SP"]
pnad2008[uf == 41, uf := "PR"]
pnad2008[uf == 42, uf := "SC"]
pnad2008[uf == 43, uf := "RS"]
pnad2008[uf == 50, uf := "MS"]
pnad2008[uf == 51, uf := "MT"]
pnad2008[uf == 52, uf := "GO"]
pnad2008[uf == 53, uf := "DF"]
table(pnad2008$uf)

## Socioeconomico -----

# Create age groups with bigger age interval
pnad2008[v8005>=0 & v8005<18, AGE :="0-17"]
pnad2008[v8005>17 & v8005<25, AGE :="18-24"]
pnad2008[v8005>24 & v8005<35, AGE :="25-34"]
pnad2008[v8005>34 & v8005<45, AGE :="35-44"]
pnad2008[v8005>44 & v8005<55, AGE :="45-54"]
pnad2008[v8005>54 & v8005<65, AGE :="55-64"]
pnad2008[v8005>64,  AGE :="65+"]
table(pnad2008$AGE)

# Create dummyvehicle
unique(pnad2008$v2032)
pnad2008[, v2032 := as.integer(v2032)]
pnad2008[v2032 %in% c(2,4,6), dummyvehicle := 1]
pnad2008[v2032 == 8, dummyvehicle := 0]
table(pnad2008$dummyvehicle,exclude = FALSE)
pnad2008[ , dummyvehicle := factor(dummyvehicle, levels = c(1,0),
                                   labels = c("Yes","No"))]


# Create  age groups with 5y intervals
pnad2008[v8005>00 & v8005<05, agegroup := "0-4"]
pnad2008[v8005>04 & v8005<14, agegroup := "5-13"]
pnad2008[v8005>13 & v8005<18, agegroup := "14-17"]
pnad2008[v8005>17 & v8005<25, agegroup := "18-24"]
pnad2008[v8005>24 & v8005<30, agegroup := "25-29"]
pnad2008[v8005>29 & v8005<35, agegroup := "30-34"]
pnad2008[v8005>34 & v8005<40, agegroup := "35-39"]
pnad2008[v8005>39 & v8005<45, agegroup := "40-44"]
pnad2008[v8005>44 & v8005<50, agegroup := "45-49"]
pnad2008[v8005>49 & v8005<55, agegroup := "50-54"]
pnad2008[v8005>54 & v8005<60, agegroup := "55-59"]
pnad2008[v8005>59 & v8005<65, agegroup := "60-64"]
pnad2008[v8005>64, agegroup := "65+"]
table(pnad2008$agegroup,exclude = FALSE)

pnad2008[v8005 >= 00 & v8005<05, agegroup1 := "0-4"]
pnad2008[v8005 >= 04 & v8005<14, agegroup1 := "5-13"]
pnad2008[v8005 >= 13 & v8005<18, agegroup1 := "14-17"]
pnad2008[v8005 >= 18 & v8005<20, agegroup1 := "18-19"]
pnad2008[v8005 >= 20 & v8005<25, agegroup1 := "20-24"]
pnad2008[v8005 >= 24 & v8005<30, agegroup1 := "25-29"]
pnad2008[v8005 >= 29 & v8005<35, agegroup1 := "30-34"]
pnad2008[v8005 >= 34 & v8005<40, agegroup1 := "35-39"]
pnad2008[v8005 >= 39 & v8005<45, agegroup1 := "40-44"]
pnad2008[v8005 >= 44 & v8005<50, agegroup1 := "45-49"]
pnad2008[v8005 >= 49 & v8005<55, agegroup1 := "50-54"]
pnad2008[v8005 >= 54 & v8005<60, agegroup1 := "55-59"]
pnad2008[v8005 >= 59 & v8005<65, agegroup1 := "60-64"]
pnad2008[v8005 >= 64, agegroup1 :="65+"]
table(pnad2008$agegroup1,exclude = FALSE)  
# Recode Sex variable, make it compatible with PNAD
pnad2008[v0302 == 2, sexo := "Masculino"]
pnad2008[v0302 == 4, sexo := "Feminino"]
table(pnad2008$sexo,exclude = FALSE)

# Recode Education variable, make it compatible with PNAD
pnad2008[v4745 == 1, edugroup_large := "Sem instrução"]
pnad2008[v4745 == 2, edugroup_large := "Fundamental incompleto ou equivalente"]
pnad2008[v4745 == 3, edugroup_large := "Fundamental completo ou equivalente"]
pnad2008[v4745 == 4, edugroup_large := "Médio incompleto ou equivalente"]
pnad2008[v4745 == 5, edugroup_large := "Médio completo ou equivalente"]
pnad2008[v4745 == 6, edugroup_large := "Superior incompleto ou equivalente"]
pnad2008[v4745 == 7, edugroup_large := "Superior completo"]
pnad2008[v4745 == 8, edugroup_large := NA]

table(pnad2008$edugroup_large,exclude = FALSE)

# Educational groups (ajuste para PNS)
pnad2008[v4745 %in% c(1,2), edugroup := "Sem instrução + Fundamental incompleto"]
pnad2008[v4745 %in% c(3,4), edugroup := "Fundamental completo"]
pnad2008[v4745 %in% c(5,6), edugroup := "Médio completo"]
pnad2008[v4745 == 7, edugroup := "Superior completo"]

table(pnad2008$edugroup,exclude = FALSE)

# Cor ou raca
# V0404	4	Cor ou raça	
#       2	Branca
#       4	Preta
#       6	Amarela
#       8	Parda
#       0	Indígena
#       9	Sem declaração

pnad2008[,v0404 := as.character(v0404)]
pnad2008[v0404 == "2", raca := "Branca"]
pnad2008[v0404 == "4", raca := "Preta"]
pnad2008[v0404 == "6", raca := "Amarela"]
pnad2008[v0404 == "8", raca := "Parda"]
pnad2008[v0404 == "0", raca := "Indígena"]
pnad2008[v0404 == "9", raca :=  NA]
table(pnad2008$raca,exclude = FALSE)

pnad2008[v0404 == "2", raca_group := "Branca"]
pnad2008[v0404 == "4" | v0404 == "8", raca_group := "Preta"]
pnad2008[v0404 == "0", raca_group := "Indígena"]
pnad2008[v0404 == "6", raca_group := "Amarela"]
pnad2008[v0404 == "9", raca_group :=  NA]
table(pnad2008$raca_group,exclude = FALSE)

# V4721		Rendimento mensal domiciliar para todas as unidades domiciliares 
# (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era 
# pensionista, empregado doméstico ou parente do empregado doméstico e das pessoas
# de menos de 10 anos de idade)

# "V4742", # Rendimento mensal domiciliar per capita 
summary(pnad2008$v4742)
# elimina observacoes missing na var.  de renda Dom per capita.
pnad2008 <- pnad2008[v4742 == 999999999999,v4742 := NA] 
summary(pnad2008$v4742)

# Create  var. income deciles of Monthly household income per capitade
# "v0105" - total moradores do domilicio
# "V4742" - Rendimento mensal domiciliar per capita 
# "v4741" - Número de componentes do domícilio (exclusive as pessoas cuja condição na unidade domiciliar era pensionista,
# empregado doméstico ou parente do empregado doméstico) 
# "v4729"
pnad2008[,v4742 := as.numeric(v4742)]
pnad2008[,v4729 := as.numeric(v4729)]
pnad2008[, decileBR:= as.numeric( cut(v4742
                                      , breaks=Hmisc::wtd.quantile(x = v4742
                                                                   , weights = v4729
                                                                   ,probs=seq(0, 1, by=0.1)
                                                                   , na.rm=T),
                                      include.lowest= TRUE, labels=1:10))]

pnad2008[,.N,by = decileBR]
pnad2008[, quintileBR:= as.numeric( cut(v4742
                                        , breaks=Hmisc::wtd.quantile(x = v4742
                                                                     , weights = v4729
                                                                     ,probs=seq(0, 1, by=0.2)
                                                                     , na.rm=T),
                                        include.lowest= TRUE, labels=1:5))]
pnad2008[,.N,by = quintileBR]

# different regions
pnad2008[, quintileRegion:= as.numeric( cut(v4742
                                            , breaks=Hmisc::wtd.quantile(x = v4742
                                                                         , weights = v4729
                                                                         ,probs=seq(0, 1, by=0.2)
                                                                         , na.rm=T),
                                            include.lowest= TRUE, labels=1:5))
         ,by = region]
pnad2008[,.N,by = quintileRegion]
pnad2008[, quartileRegion:= as.numeric( cut(v4742
                                            , breaks=Hmisc::wtd.quantile(x = v4742
                                                                         , weights = v4729
                                                                         ,probs=seq(0, 1, by=0.25)
                                                                         , na.rm=T),
                                            include.lowest= TRUE, labels=1:4))
         ,by = region]
pnad2008[,.N,by = quartileRegion]

#  Metro Areas
pnad2008[, quintileMetro:= as.numeric( cut(v4742
                                           , breaks=Hmisc::wtd.quantile(x = v4742
                                                                        , weights = v4729
                                                                        ,probs=seq(0, 1, by=0.2)
                                                                        , na.rm=T),
                                           include.lowest= TRUE, labels=1:5))
         ,by = metro]
pnad2008[,.N,by = quintileMetro]
pnad2008[, quartileMetro:= as.numeric( cut(v4742
                                           , breaks=Hmisc::wtd.quantile(x = v4742
                                                                        , weights = v4729
                                                                        ,probs=seq(0, 1, by=0.25)
                                                                        , na.rm=T),
                                           include.lowest= TRUE, labels=1:4))
         ,by = metro]
pnad2008[,.N,by = quartileMetro]
# 2-Way Frequency Table -  Numero de casos dentro de cada Decil tem que ser igual/proximo

# table(pnad2008$quintileRegion, pnad2008$region)
# table(pnad2008$quartileRegion, pnad2008$region)
# 
# table(pnad2008$quintileMetro, pnad2008$metro)
# table(pnad2008$quartileMetro, pnad2008$metro)
# 
# table(pnad2008$quartileMetro, pnad2008$dummyVehicle, pnad2008$metro)

pnad2008[,table(quintileBR),by = c("region")]
pnad2008[,table(quartileRegion),by = c("region")]
pnad2008[,table(quintileMetro),by = c("metro")]
pnad2008[,table(quartileMetro),by = c("metro")]
pnad2008[,table(dummyvehicle),by = c("metro","quartileMetro")]


names(pnad2008)

# limpa memoria
gc(reset = T) 

## Mobility ------


# v1410  Costuma ir a pé ou de bicicleta de casa para o trabalho
# 2	Sim
# 4	Não
# "" - Não aplicável 
#
# V1411	11	Tempo gasto para ir e voltar do trabalho
# 1	Menos de 10 minutos
# 2	10 a 19 minutos
# 3	20 a 29 minutos
# 4	30 a 44 minutos
# 5	45 a 59 minutos
# 6	60 minutos ou mais 
# Não aplicável
class(pnad2008$v1411)
pnad2008[,actv_commutetime_00to09 := fifelse(v1411 == "1" & v1410 == "2",1,0)] # Menos de 10 minutos
pnad2008[,actv_commutetime_10to19 := fifelse(v1411 == "2" & v1410 == "2",1,0)] # 10 a 19 minutos
pnad2008[,actv_commutetime_20to29 := fifelse(v1411 == "3" & v1410 == "2",1,0)] # 20 a 29 minutos
pnad2008[,actv_commutetime_30to44 := fifelse(v1411 == "4" & v1410 == "2",1,0)] # 30 a 44 minutos
pnad2008[,actv_commutetime_45to59 := fifelse(v1411 == "5" & v1410 == "2",1,0)] # 45 a 59 minutos
pnad2008[,actv_commutetime_from60 := fifelse(v1411 == "6" & v1410 == "2",1,0)] # 60 minutos ou mais 
pnad2008[,actv_commutetime_from30 := fifelse(v1411 %in% c("4","5","6") & v1410 == "2",1,0)] # 30 minutos ou mais 

unique(pnad2008$v1410)

# v9054
# Tipo de estabelecimento ou onde era exercido o trabalho principal da semana de referência
# 3	No domicílio em que morava
pnad2008[v1410 == 0, v1410 := NA]
pnad2008[v9054 == 3, v1410 := 4]
pnad2008[v1410 == "2", v1410 := "Sim"]
pnad2008[v1410 == "4", v1410 := "Não"]
pnad2008[v1410 == "", v1410 := NA]
table(pnad2008$v1410,exclude = FALSE)

# V4610: Inversao da fracao
pnad2008[,v4610 := as.numeric(v4610)]
pnad2008[, pre_wgt  := v4619 * v4610]
summary(pnad2008$pre_wgt)

# vehicle ownership variable, make it compatible with PNAD
# V2032	32a	Tem carro ou motocicleta de uso pessoal	2	Carro
# 4	Motocicleta
# 6	Carro e motocicleta
# 8	Não 
# Não aplicável

unique(pnad2008$v2032)
pnad2008[,v2032 := as.numeric(v2032)]
pnad2008[v2032 == 2, vehicleOwnership := "Automóvel"]
pnad2008[v2032 == 4, vehicleOwnership := "Motocicleta"]
pnad2008[v2032 == 6, vehicleOwnership := "Automóvel + Motocicleta"]
pnad2008[v2032 == 8, vehicleOwnership := "Nenhum"]
pnad2008[is.na(v2032), vehicleOwnership := NA]
table(pnad2008$v2032,exclude = FALSE)
table(pnad2008$vehicleOwnership,exclude = FALSE)



# Save Pnad2008 files ------------

dir.create("../../data/transporte_ativo_2008-2019/")
readr::write_rds(x = pnad2008
                 ,file = "../../data/transporte_ativo_2008-2019/pnad2008.rds"
                 ,compress = "gz")
# readr::write_rds(x = tmp_dom
#                  , file = "../../data/transporte_ativo_2008-2019/pnad2008dom.rds"
#                  ,compress = "gz")

