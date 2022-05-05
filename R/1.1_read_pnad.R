666 - add weighted_quantile
# Load libraries -----

# https://www.ibge.gov.br/estatisticas/sociais/educacao/9127-pesquisa-nacional-por-amostra-de-domicilios.html?edicao=18338&t=microdados
rm(list=ls())
gc(reset=TRUE)
library(PNADcIBGE) # devtools::install_github("Gabriel-Assuncao/PNADcIBGE")
library(survey)
library(srvyr)
library(microdadosBrasil) # devtools::install_github("lucasmation/microdadosBrasil")
# # Downloading data
# source("R/0.1_parse_sasci.R") #

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
pnad2008 <- tmp_pes[tmp_dom,on = "V0102"]
table(tmp_pes$V0102)
pnad2008 <- data.table::merge.data.table(x = tmp_pes
                                         ,y = tmp_dom
                                         , by = c("V0101", "V0102", "V0103","UF")
                                         , all = TRUE)

# Clean memory
rm(list=setdiff(ls(), c("pnad2008", "pnad2008dom")))
gc(reset = T)





# Add VARiables to pnad2008 ---------------------

# year variable     
pnad2008[, year := 2008]
colnames(pnad2008) <- tolower(colnames(pnad2008))

# # Count variable     
# pnad2008[, vcount := 1]
# table(pnad2008$vcount)

# Create Great Geographical Regions [data.table]
pnad2008[uf < 20, region :="North"]
pnad2008[uf > 19 & uf < 30, region :="Northeast"]
pnad2008[uf > 29 & uf < 40, region :="Southwest"]
pnad2008[uf > 39 & uf < 50, region :="South"]
pnad2008[uf > 49 & uf < 60, region :="Midwest"]
table(pnad2008$region)


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
pnad2008[v8005<5, agegroup := "0-4"]
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
table(pnad2008$agegroup)


# Recode Urban vs Rural variable
pnad2008[v4105<4, urban := "Urban"]
pnad2008[v4105>3 & v4105<9, urban := "Rural"]
table(pnad2008$urban)


# Recode Sex variable, make it compatible with PNAD
pnad2008[v0302 == 2, v0302 := "Men"]
pnad2008[v0302 == 4, v0302 := "Women"]
table(pnad2008$v0302)

# Recode Education variable, make it compatible with PNAD
pnad2008[v4745 == 1, v4745 := "Uneducated"]
pnad2008[v4745 == 2, v4745 := "Incomplete primary school"]
pnad2008[v4745 == 3, v4745 := "Complete primary school"]
pnad2008[v4745 == 4, v4745 := "Incomplete high school"]
pnad2008[v4745 == 5, v4745 := "Complete high school"]
pnad2008[v4745 == 6, v4745 := "Incomplete university degree"]
pnad2008[v4745 == 7, v4745 := "University degree"]
pnad2008[v4745 == 8, v4745 := NA]
table(pnad2008$v4745)



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
pnad2008[uf == 35, uf_name :="Sao Paulo"]
pnad2008[uf == 41, uf_name :="Parana"]
pnad2008[uf == 42, uf_name :="Santa Catarina"]
pnad2008[uf == 43, uf_name :="Rio Grande do Sul"]
pnad2008[uf == 50, uf_name :="Mato Grosso do Sul"]
pnad2008[uf == 51, uf_name :="Mato Grosso"]
pnad2008[uf == 52, uf_name :="Goias"]
pnad2008[uf == 53, uf_name :="Federal District"]
table(pnad2008$uf_name)



# Create variable Metropolitan area
pnad2008[v4727 !=1, metro :="Non-metropolitan area"]
pnad2008[uf == 15 & v4727 == 1, metro := "Belem"]
pnad2008[uf == 23 & v4727 == 1, metro := "Fortaleza"]
pnad2008[uf == 26 & v4727 == 1, metro := "Recife"]
pnad2008[uf == 29 & v4727 == 1, metro := "Salvador"]
pnad2008[uf == 31 & v4727 == 1, metro := "Belo Horizonte"]
pnad2008[uf == 33 & v4727 == 1, metro := "Rio de Janeiro"]
pnad2008[uf == 35 & v4727 == 1, metro := "Sao Paulo"]
pnad2008[uf == 41 & v4727 == 1, metro := "Curitiba"]
pnad2008[uf == 43 & v4727 == 1, metro := "Porto Alegre"]
pnad2008[uf == 53 & v4727 == 1, metro := "Federal District"]
table(pnad2008$metro)

gc(reset = T)



# Delete Missing values from Income variable (v4721 Monthly household income per capita)
# to create new variable of income deciles
#Antes da limpeza==391.868 obs. A lipeza tirou 11.573 cases -> 380.295

# 99999
# Joao:
# "V4742", # Rendimento mensal domiciliar per capita 
summary(pnad2008$v4742)
pnad2008 <- pnad2008[v4742 > 0 & v4742 < 1.000e+11 & !is.na(v4742), ] #elimina observacoes missing na var.  de renda Dom per capita.
summary(pnad2008$v4742)

# Create  var. income deciles of Monthly household income per capitade
pnad2008[, decileBR:= as.numeric( cut(v4742, breaks=quantile(v4742,
                                                             probs=seq(0, 1, by=0.1), na.rm=T),
                                      include.lowest= TRUE, labels=1:10))]

# Checking Table
table(pnad2008$decileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo


# Create  var. income quintile of Monthly household income per capitade


pnad2008[, quintileBR:= as.numeric( cut(v4742, breaks=quantile(v4742,
                                                               probs=seq(0, 1, by=0.2), na.rm=T),
                                        include.lowest= TRUE, labels=1:5))]


# function to Create Quintile for different regions
pnad2008[, quintileRegion:= as.numeric( cut(v4742, breaks=quantile(v4742,
                                                               probs=seq(0, 1, by=0.2), na.rm=T),
                                        include.lowest= TRUE, labels=1:5))
         ,by = region]


# function to Create Quartile for different regions
pnad2008[, quartileRegion:= as.numeric( cut(v4742, breaks=quantile(v4742,
                                                                   probs=seq(0, 1, by=0.25), na.rm=T),
                                            include.lowest= TRUE, labels=1:4))
         ,by = region]


# function to Create Quintile for different Metro Areas
pnad2008[, quintileMetro:= as.numeric( cut(v4742, breaks=quantile(v4742,
                                                                  probs=seq(0, 1, by=0.2), na.rm=T),
                                           include.lowest= TRUE, labels=1:5))
         ,by = metro]

# function to Create Quartile for different Metro Areas
pnad2008[, quartileMetro:= as.numeric( cut(v4742, breaks=quantile(v4742,
                                                                   probs=seq(0, 1, by=0.25), na.rm=T),
                                            include.lowest= TRUE, labels=1:4))
         ,by = metro]

# create regional income deciles, quintiles and quartiles
# pnad2008 <- do.call(rbind, lapply(split(pnad2008, pnad2008[, region]), funQuintReg)) ; gc(reset = T)
# pnad2008 <- do.call(rbind, lapply(split(pnad2008, pnad2008[, region]), funQuartReg)) ; gc(reset = T)
# pnad2008 <- do.call(rbind, lapply(split(pnad2008, pnad2008[, metro]), funQuintMetro)) ; gc(reset = T)
# pnad2008 <- do.call(rbind, lapply(split(pnad2008, pnad2008[, metro]), funQuartMetro)) ; gc(reset = T)

head(pnad2008)

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

# Create modified commute and Active Travel variables 
# # Para pessoas ocupadas e q nao responderam a v1410, imputa q desloc ativo=0
# # Porque?: Se faz isso apenas para elas contarem no denominador na hora de calcular a proporcao de desloca ativo
# # inclui na var v1410 1.393 casos 

# Impute commute time '0' no active commute IF person works from home
pnad2008[v9054 == 3, v9057mod := NA]
pnad2008[, v9057mod := as.numeric(v9057)]

unique(pnad2008$v9057)
unique(pnad2008$v9057mod)
table(pnad2008$v9057mod)
table(pnad2008$v9057)

# Impute no active commute IF person works from home, or 
# has a job but did not answered the question of active travel (probably due to health issues)
pnad2008[v1410 == 0, v1410 :=NA]
pnad2008[, v1410mod := ifelse(v9054 == 3, 4, as.numeric(v1410))]

table(pnad2008$v1410)
unique(pnad2008$v1410)
table(pnad2008$v1410mod)
unique(pnad2008$v1410mod)


#  Impute active commute time '0' IF person works from home
pnad2008[v1411 == 0, v1411 := NA]
pnad2008[, v1411 := as.numeric(v1411)]
pnad2008[, v1411mod := ifelse(v9054 == 3, NA, v1411)]

table(pnad2008$v1411,exclude = FALSE)
unique(pnad2008$v1411)
table(pnad2008$v1411mod,exclude = FALSE)
unique(pnad2008$v1411mod)


# create indicator variable of ind. that practice active travel for > 30minutes when commuting to work
unique(pnad2008$v1410)
unique(pnad2008$v1410mod)
pnad2008[v1410mod == 2 & v1411 > 3, actv_commutetime30 := 1]
pnad2008[v1410mod == 2 & v1411 <= 3, actv_commutetime30 := 0]
pnad2008[v1410mod == 1 | is.na(v1410mod), actv_commutetime30 := NA]
table(pnad2008$actv_commutetime30,exclude = FALSE)


# now create the pre-stratified weight to be used in all of the survey designs
# V4619: Fator de sub-amostragem
# V4610: Inversao da fracao
pnad2008[,v4610 := as.numeric(v4610)]
pnad2008[, pre_wgt  := v4619 * v4610]
summary(pnad2008$pre_wgt)


#Recode Active Travel variable P040 into string
unique(pnad2008$v1410)
pnad2008[v1410 == "2", v1410 := "Yes"]
pnad2008[v1410 == "4", v1410 := "No"]
pnad2008[v1410 == "", v1410 := NA]
table(pnad2008$v1410,exclude = FALSE)

# vehicle ownership variable, make it compatible with PNAD
unique(pnad2008$v2032)
pnad2008[,v2032 := as.numeric(v2032)]
pnad2008[v2032 == 2, vehicleOwnership := "Car"]
pnad2008[v2032 == 4, vehicleOwnership := "Motorcycle"]
pnad2008[v2032 == 6, vehicleOwnership := "Car + Motorcycle"]
pnad2008[v2032 == 8, vehicleOwnership := "None"]
pnad2008[is.na(v2032), vehicleOwnership := NA]
table(pnad2008$v2032,exclude = FALSE)
table(pnad2008$vehicleOwnership,exclude = FALSE)

names(pnad2008)

# limpa memoria
gc(reset = T) 

# Recode Housegold DAta  ----------------


tmp_dom[, year := 2008]
colnames(tmp_dom) <- tolower(colnames(tmp_dom))
tmp_dom[uf < 20, region := "North"]
tmp_dom[uf > 20 & uf < 30, region := "Northeast"]
tmp_dom[uf > 30 & uf < 40, region := "Southeast"]
tmp_dom[uf > 40 & uf < 50, region := "South"]
tmp_dom[uf > 50 & uf < 60, region := "Midwest"]
table(tmp_dom$region)

# Recode Urban vs Rural variable
tmp_dom[v4105<4, urban := "Urban"]
tmp_dom[v4105>3 & v4105<9, urban := "Rural"]
table(tmp_dom$urban,exclude = FALSE)



# vehicle ownership variable, make it compatible with PNAD
unique(tmp_dom$v2032)
tmp_dom[,v2032 := as.numeric(v2032)]
tmp_dom[v2032 == 2, vehicleOwnership := "Car"]
tmp_dom[v2032 == 4, vehicleOwnership := "Motorcycle"]
tmp_dom[v2032 == 6, vehicleOwnership := "Car + Motorcycle"]
tmp_dom[v2032 == 8, vehicleOwnership := "None"]
tmp_dom[is.na(v2032), vehicleOwnership := NA]
table(tmp_dom$v2032,exclude = FALSE)
table(tmp_dom$vehicleOwnership,exclude = FALSE)

# Create dummyvehicle
unique(tmp_dom$v2032)
tmp_dom[, v2032 := as.integer(v2032)]
tmp_dom[v2032 %in% c(2,4,6), dummyvehicle := 1]
tmp_dom[v2032 == 8, dummyvehicle := 0]
table(tmp_dom$dummyvehicle,exclude = FALSE)
tmp_dom[ , dummyvehicle := factor(dummyvehicle, levels = c(1,0),
                                   labels = c("Yes","No"))]

# Save Pnad2008 files ------------

dir.create("../../data/transporte_ativo_2008-2019/")
readr::write_rds(x = pnad2008
                   ,file = "../../data/transporte_ativo_2008-2019/pnad2008.rds"
                   ,compress = "gz")
readr::write_rds(x = tmp_dom
                 , file = "../../data/transporte_ativo_2008-2019/pnad2008dom.rds"
                 ,compress = "gz")

