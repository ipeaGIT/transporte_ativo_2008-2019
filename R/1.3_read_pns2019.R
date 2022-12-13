# 1. Load packages  ------------------------------------------------------
rm(list=ls())
gc(reset=TRUE)
library(data.table)
library(magrittr)

# Read the .txt file

load("data-raw/pns2019.RData")
data.table::setDT(pns2019)

pns2019_rds <- readr::read_rds("../../data-raw/PNS/2019/pns2019.rds")
data.table::setDT(pns2019_rds)

# Add
# Le c("V0029", "V00291", "V00292", "V00293", "V0030", "V00301", 
#    "V00302", "V00303")

# extra_2019 <- PNSIBGE::read_pns(
#   microdata  = "L:/Proj_acess_oport/data-raw/PNS/2019/pns_2019_20220525/PNS_2019.txt"
#   ,input_txt = "L:/Proj_acess_oport/data-raw/PNS/2019/input_PNS_2019.sas"
#   #,vars = c("V0006_PNS","V0029", "V00291"
#   #          , "V00292", "V00293"
#             )
# )
# data.table::setDT(extra_2019)
# 
# pns2019 <- pns2019[
#   extra_2019
#   , on = c("C00301","ID_DOMICILIO") 
#   , ":="(V0029 = i.V0029     # Peso do morador selecionado sem calibracao
#          ,V00291 = i.V00291  # Peso do morador selecionado com calibracao
#          ,V00292 = i.V00292  # Projeção da população para moradores selecionados
#          ,V00293 = i.V00293) # Domínio de projeção para morador selecionado
# ]
# 
# clean memory
gc(reset = T)

## Definição do peso e filtragem de respondentes do questionario ----
# Selecionando registros válidos para o módulo P e calculando peso amostral 
## - summary de verificação

sum(as.numeric(pns2019$V0006_PNS),na.rm = TRUE)
sum(as.numeric(pns2019$V0028),na.rm = TRUE)
sum(as.numeric(pns2019$V0029),na.rm = TRUE)
sum(as.numeric(pns2019$V0030),na.rm = TRUE)
sum(as.numeric(pns2019$V00281),na.rm = TRUE)
sum(as.numeric(pns2019$V00282),na.rm = TRUE)
sum(as.numeric(pns2019$V00291),na.rm = TRUE)
sum(as.numeric(pns2019$V00292),na.rm = TRUE)
sum(as.numeric(pns2019$V00293),na.rm = TRUE)
sum(as.numeric(pns2019$V00301),na.rm = TRUE)
sum(as.numeric(pns2019$V00302),na.rm = TRUE)
sum(as.numeric(pns2019$VDC001),na.rm = TRUE)
sum(as.numeric(pns2019$VDC003),na.rm = TRUE)

# --

#pns2019 <- pns2019[]

soma_v00291 <- sum(as.numeric(pns2019[V0025A == "1"]$V00291),na.rm = TRUE) # 168426190
nobs_pns2019 <- nrow(pns2019[V0025A == "1"]) # 94114

pns2019[,peso_morador_selec := (V00291 * nobs_pns2019) / soma_v00291 ]


summary(pns2019$peso_morador_selec)
# 4 RENAME variables=====
## 4.0 Socioeconomic  ----------------

# Recode Sex Variable, make it compatible with PNAD
# C006	C6	Sexo
pns2019[C006 == 1, sexo := "Masculino"]
pns2019[C006 == 2, sexo := "Feminino"]
table(pns2019$sexo,exclude = FALSE)
table(pns2019$C006,exclude = FALSE)


# Vehicle ownership Variable, make it compatible with PNAD
# A018025 - Neste domicilio existe motocicleta? ( 1 = sim, 2= nao)
# A018026 - Quantos? (01-98 = motos,99 = ignorado,"" = Nao aplicavel)
# A018027 - Quantos carros tem este domicílio?  ( 1 = sim, 2= nao)
# A018028 - Quantos? (01-98 = carros,99 = ignorado,"" = Nao aplicavel)
pns2019[A018025 == "2" & A018027 == "1", vehicleOwnership := "Carro"]      #  2
pns2019[A018025 == "1" & A018027 == "2", vehicleOwnership := "Motocicleta"] #  4 
pns2019[A018025 == "1" & A018027 == "1", vehicleOwnership := "Carro + Motocicleta"] # 6 
pns2019[A018025 == "2" & A018027 == "2", vehicleOwnership := "Nenhum"] # 8
table(pns2019$vehicleOwnership,exclude = FALSE)


# Dummy for Vehicle ownership Variable, make it compatible with PNAD
# If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit
pns2019[, dummyVehicle := data.table::fifelse(vehicleOwnership == "Nenhum", 0, 1)]

pns2019[,dummyVehicle := factor(dummyVehicle, levels = c(1,0),
                                labels = c("Sim","Não"))]

# Create  age groups with 5y intervals
# C008	C8	Idade do morador na data de referência
pns2019[C008 >= 00 & C008<05, agegroup := "0-4"]
pns2019[C008 >= 04 & C008<14, agegroup := "5-13"]
pns2019[C008 >= 13 & C008<18, agegroup := "14-17"]
pns2019[C008 >= 17 & C008<25, agegroup := "18-24"]
pns2019[C008 >= 24 & C008<30, agegroup := "25-29"]
pns2019[C008 >= 29 & C008<35, agegroup := "30-34"]
pns2019[C008 >= 34 & C008<40, agegroup := "35-39"]
pns2019[C008 >= 39 & C008<45, agegroup := "40-44"]
pns2019[C008 >= 44 & C008<50, agegroup := "45-49"]
pns2019[C008 >= 49 & C008<55, agegroup := "50-54"]
pns2019[C008 >= 54 & C008<60, agegroup := "55-59"]
pns2019[C008 >= 59 & C008<65, agegroup := "60-64"]
pns2019[C008 >= 64, agegroup :="65+"]

table(pns2019$agegroup,exclude = FALSE)  

# Create age groups with bigger age interval
pns2019[C008 >=0 & C008<18, AGE :="0-17"]
pns2019[C008 >17 & C008<25, AGE :="18-24"]
pns2019[C008 >=25 & C008<35, AGE :="25-34"]
pns2019[C008 >=35 & C008<45, AGE :="35-44"]
pns2019[C008 >=45 & C008<55, AGE :="45-54"]
pns2019[C008 >=55 & C008<65, AGE :="55-64"]
pns2019[C008 >=65,  AGE :="65+"]

table(pns2019$AGE,exclude = FALSE)  

# Create BMI  - Body Max Index (weight / height)
# P00104	P1	Se sim, qual o peso (kg)?
# P00404	P4	Especifique a altura em cm (P004)

pns2019[,P00104 := as.numeric(P00104)] # weight
pns2019[,P00404 := as.numeric(P00404)] # height
summary(pns2019$P00104) # weight
summary(pns2019$P00404) # height
pns2019[, P00404 := ifelse(P00404==0 , NA, P00404/100)] # If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit

#compute BMI    
pns2019[, bmi := P00104 / (P00404^2) ]
summary(pns2019$bmi)

# Recode Education Variable, make it compatible with PNAD
# VDD004A		Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade) -  SISTEMA DE 8 ANOS
pns2019[VDD004A == 1, edugroup_large := "Sem instrução"]
pns2019[VDD004A == 2, edugroup_large := "Fundamental incompleto ou equivalente"]
pns2019[VDD004A == 3, edugroup_large := "Fundamental completo ou equivalente"]
pns2019[VDD004A == 4, edugroup_large := "Médio incompleto ou equivalente"]
pns2019[VDD004A == 5, edugroup_large := "Médio completo ou equivalente"]
pns2019[VDD004A == 6, edugroup_large := "Superior incompleto ou equivalente"]
pns2019[VDD004A == 7, edugroup_large := "Superior completo"]

table(pns2019$edugroup_large,exclude = FALSE)

# Educational groups
pns2019[VDD004A < 3        , edugroup := "Sem instrução + Fundamental incompleto"]
pns2019[VDD004A %in% c(3,4), edugroup := "Fundamental completo"]
pns2019[VDD004A %in% c(5,6), edugroup := "Médio completo"]
pns2019[VDD004A == 7       , edugroup := "Superior completo"]

table(pns2019$edugroup,exclude = FALSE)



# Recode Race variable into string
# C009	C9	Cor ou raça

unique(pns2019$C009)
pns2019[,C009 := as.character(C009)]
pns2019[C009 == "1", raca := "Branca"]
pns2019[C009 == "2", raca := "Preta"]
pns2019[C009 == "3", raca := "Amarela"]
pns2019[C009 == "4", raca := "Parda"]
pns2019[C009 == "5", raca := "Indígena"]
pns2019[C009 == "9", raca :=  NA]
table(pns2019$raca,exclude = FALSE)
table(pns2019$C009,exclude = FALSE)

pns2019[!is.na(P040) & V0026 == 1,.N,by =.(edugroup,raca)]

# Raca group
pns2019[, raca_group :=  raca]
pns2019[raca == "Preta", raca_group := "Negra"]
pns2019[raca == "Parda", raca_group := "Negra"]
table(pns2019$raca_group,exclude = FALSE)



pns2019[,.N,by = .(sexo,edugroup,P040)][order(edugroup)]
## 4.1 Regions ----

# Recode Urban Rural Variable, make it compatible with PNAD
# V0026		Tipo de situação censitária
pns2019[V0026 == 1 , urban := "Urbano"]
pns2019[V0026 == 2 , urban := "Rural"]
table(pns2019$urban,exclude = FALSE)


# V0001	     	Grandes Regioes
pns2019[,V0001 := as.numeric(V0001)]
pns2019[V0001 < 20, region :="Norte"]
pns2019[V0001 > 19 & V0001 < 30, region :="Nordeste"]
pns2019[V0001 > 29 & V0001 < 40, region :="Sudeste"]
pns2019[V0001 > 39 & V0001 < 50, region :="Sul"]
pns2019[V0001 > 49 & V0001 < 60, region :="Centro-Oeste"]
table(pns2019$region,exclude = FALSE)  

# UF Variable
# V0001		Unidade da Federação

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

table(pns2019$uf,exclude = FALSE)
table(pns2019$uf_name,exclude = FALSE)


# Recode Metropolitan area Variable, make it compatible with PNAD
# V0031		Tipo de área	
# 1	Capital
# 2	Resto da RM (Região Metropolitana, excluindo a capital)
# 3	RIDE (excluindo a capital)
# 4	Resto da UF (Unidade da Federação, excluindo a região metropolitana e RIDE)
pns2019[V0031 == 1 | V0031 == 2, v4727 := 1]
pns2019[V0031 > 2, v4727 := 3]


# Create Variable Metropolitan area
# V0031		Tipo de área	
pns2019[V0031 == 4             , metro := "Restante das UF"]
pns2019[V0001 == 15 & V0031 <= 3, metro := "Belém"]
pns2019[V0001 == 23 & V0031 <= 3, metro := "Fortaleza"]
pns2019[V0001 == 26 & V0031 <= 3, metro := "Recife"]
pns2019[V0001 == 29 & V0031 <= 3, metro := "Salvador"]
pns2019[V0001 == 31 & V0031 <= 3, metro := "Belo Horizonte"]
pns2019[V0001 == 33 & V0031 <= 3, metro := "Rio de Janeiro"]
pns2019[V0001 == 35 & V0031 <= 3, metro := "São Paulo"]
pns2019[V0001 == 41 & V0031 <= 3, metro := "Curitiba"]
pns2019[V0001 == 43 & V0031 <= 3, metro := "Porto Alegre"]
pns2019[V0001 == 53 & V0031 <= 3, metro := "Distrito Federal"]

table(pns2019$metro,exclude = FALSE)

pns2019[,country := "Brasil"]
pns2019[,dummyMetro := fifelse(metro == "Restante das UF","Non-metro","Metro")]

table(pns2019$dummyMetro,useNA = "always")

## 4.2 Mobility -----
# create indicator variable of ind. above 18yearsold that practice active travel for > 30minutes
# this is the definition used in table 3.4.1.1 of IBGE report
#
# P04101	Quantas horas o(a) Sr(a) gasta para percorrer este trajeto a pé ou de bicicleta, 
# considerando a ida e a volta do trabalho?
# P04102	Quantos minutos o(a) Sr(a) gasta para percorrer este trajeto a pé ou de bicicleta,
# considerando a ida e a volta do trabalho?
# P04301	No dia que o(a) Sr(a) faz esta atividade, quantas horas o(a) Sr(a) gasta no 
# deslocamento a pé ou de   bicicleta, considerando a ida e a volta?
# P04302	P43	No dia que o(a) Sr(a) faz esta atividade, quantos minutos o(a) Sr(a) gasta
# no deslocamento a pé ou de   bicicleta, considerando a ida e a volta?
table(pns2019$P040)
summary(pns2019$P04101)
summary(pns2019$P04102)
summary(pns2019$P04301)
summary(pns2019$P04302)
pns2019[,P04101 := as.numeric(P04101)]
pns2019[,P04102 := as.numeric(P04102)]
pns2019[, actv_commutetime := P04101 * 60 + P04102] # Active commute time

summary(pns2019$actv_commutetime)

# making it compatible with PNAD2008
pns2019[,actv_commutetime_00to09 := fifelse(actv_commutetime >= 01 & actv_commutetime <= 09,1,0)] # Menos de 10 minutos
pns2019[,actv_commutetime_10to19 := fifelse(actv_commutetime >= 10 & actv_commutetime <= 19,1,0)] # 10 a 19 minutos
pns2019[,actv_commutetime_20to29 := fifelse(actv_commutetime >= 20 & actv_commutetime <= 29,1,0)] # 20 a 29 minutos
pns2019[,actv_commutetime_30to44 := fifelse(actv_commutetime >= 30 & actv_commutetime <= 44,1,0)] # 30 a 44 minutos
pns2019[,actv_commutetime_45to59 := fifelse(actv_commutetime >= 45 & actv_commutetime <= 59,1,0)] # 45 a 59 minutos
pns2019[,actv_commutetime_from60 := fifelse(actv_commutetime >= 60,1,0)] # 60 minutos ou mais 
pns2019[,actv_commutetime_from30 := fifelse(actv_commutetime >= 30,1,0)] # 30 minutos ou mais 

# Recode Acctive Travel Variable P040 into string
pns2019[, P040 := as.character(P040)]
pns2019[(P040 == "1" | P040 == "2") & is.na(actv_commutetime), P040 :=  NA]
pns2019[(P040 == "1" | P040 == "2") & actv_commutetime == 0, P040 :=  NA]
pns2019[P040 == "1", P040 := "Sim, todo o trajeto"]
pns2019[P040 == "2", P040 := "Sim, parte do trajeto"]
pns2019[P040 == "3", P040 :=  "Não"]

# Recode Active Travel Variable, make it compatible with PNAD
# P040	-	Para ir ou voltar do trabalho, o(a) Sr(a) faz algum trajeto a pé ou de bicicleta
# 1	Sim, todo o trajeto
# 2	Sim, parte do trajeto
# 3	Não
# Não aplicável

pns2019[P040 %like% "Sim", v1410 := "Sim"]
pns2019[P040 == "Não", v1410 := "Não"]
table(pns2019$v1410,exclude = FALSE)
table(pns2019$P040,exclude = FALSE)

# year variable     
pns2019[, year := 2019]

## 4.3. Income  ----

# E01602	E16	Qual era o rendimento bruto mensal ou retirada que ________________ fazia normalmente nesse trabalho? - Valor em dinheiro (R$)
# E01604	E16	Qual era o rendimento bruto mensal ou retirada que ________________ fazia normalmente nesse trabalho? - Valor estimado dos produtos ou mercadorias (R$)
# E01802	E18	Qual era o rendimento bruto mensal ou retirada que ________________ fazia normalmente nesse(s) outro(s) trabalho(s)? - Valor em dinheiro (R$)
# E01804	E18	Qual era o rendimento bruto mensal ou retirada que ________________ fazia normalmente nesse(s) outro(s) trabalho(s)? - Valor estimado dos produtos ou mercadorias (R$)
# F001021	F1	Valor recebido em reais (F001)
# F007021	F7	Valor recebido em reais (F007)
# F00802	F8	Valor recebido em reais (F008)
# VDF00102	F8	Valor recebido em reais (VDF001)


# V0026 total de moradores 
pns2019[,V0026 := as.numeric(V0026)]
summary(pns2019$V0026)


# Summary of income variables
summary(pns2019$E01602)
summary(pns2019$E01604)
summary(pns2019$E01802)
summary(pns2019$E01804)
summary(pns2019$F001021)
summary(pns2019$F007021)
pns2019[,F008021 := as.numeric(F008021)]
summary(pns2019$F008021)
summary(pns2019$VDF00102)

#  Household Income per Capita, compatible with PNAD 2008 data

pns2019[ C004 <17 , v4742 := sum( E01602, E01604, E01802,
                                  E01804, F001021, F007021, 
                                  F008021, VDF00102, na.rm = T) / as.numeric(V0026),
         by= .(V0001, V0024, UPA_PNS, V0006_PNS)] # sum all income sources


summary(pns2019$v4742)
# joao | # > Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# joao | # >    0    1022    2196    3828    4269  644998   172 
# rafa | # > Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# rafa | # >   0     340      670    1140    1190  146000  130415 
# 
# # 1119 casos com RDPC igual a 0
head(table(pns2019$v4742))
summary(pns2019$v4742)


########### Create income quantiles
# add ranks from
# <https://stackoverflow.com/questions/16184947/cut-error-breaks-are-not-unique>
#
# Create  var. income deciles of Monthly household income per capita
function_ranks <- function(x,w,p,label = FALSE){
  x_ranks <- rank(x, ties.method = "first")
  distr <- cut(x = x_ranks
               ,breaks = Hmisc::wtd.quantile(x = x_ranks
                                             , weights = w
                                             , probs = p
                                             , na.rm=T)
               , include.lowest=TRUE, labels = label)
  return(as.numeric(distr))
}
pns2019[, decileBR:= function_ranks(x = v4742,w = V00291
                                    ,p = seq(0, 1, by=0.1)
                                    ,label = 1:10)]
 pns2019[, decileBR:= as.numeric( cut(v4742
                                      , breaks = Hmisc::wtd.quantile(x = v4742
                                                                     , weights = V00291
                                                                     , probs = seq(0, 1, by=0.1)
                                                                     , na.rm=T),
                                      include.lowest = TRUE, labels = 1:10))]
# Checking Table
table(pns2019$decileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo


# Create  var. income quintile of Monthly household income per capitade
pns2019[, quintileBR:= as.numeric( cut(v4742
                                       , breaks = Hmisc::wtd.quantile(x = v4742
                                                                      , weights = V00291
                                                                      ,probs=seq(0, 1, by=0.2)
                                                                      , na.rm=T),
                                       include.lowest= TRUE, labels=1:5))]
#pns2019[, quintileBR:= function_ranks(x = v4742,w = V00291
#                                    ,p = seq(0, 1, by=0.2)
#                                    ,label = 1:5)]
table(pns2019$quintileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo
table(pns2019$quintileBR1) 

# function to Create Quintile for different regions
pns2019[, quintileRegion:= as.numeric( cut(v4742
                                           , breaks = Hmisc::wtd.quantile(x = v4742
                                                                          , weights = V00291
                                                                          ,probs=seq(0, 1, by=0.2)
                                                                          , na.rm=T),
                                           include.lowest= TRUE, labels=1:5)), by = region]
#pns2019[, quintileRegion := function_ranks(x = v4742,w = V00291
#                                      ,p = seq(0, 1, by=0.2)
#                                      ,label = 1:5), by = region]

# function to Create Quartile for different regions
pns2019[, quartileRegion:= as.numeric( cut(v4742
                                           , breaks = Hmisc::wtd.quantile(x = v4742
                                                                          , weights = V00291
                                                                          ,probs=seq(0, 1, by=0.25), na.rm=T),
                                           include.lowest= TRUE, labels=1:4)), by = region]


# function to Create Quintile for different Metro Areas
pns2019[, quintileDummyMetro:= as.numeric( cut(v4742,
                                               breaks = Hmisc::wtd.quantile(x = v4742
                                                                            , weights = V00291
                                                                            , probs = seq(0, 1, by=0.2), na.rm=T)
                                               , include.lowest= TRUE, labels=1:5)), by = dummyMetro]
#pns2019[, quintileDummyMetro := function_ranks(x = v4742,w = V00291
#                                           ,p = seq(0, 1, by=0.2)
#                                           ,label = 1:5), by = dummyMetro]

# function to Create Quintile for different Metro Areas
pns2019[, quintileMetro:= as.numeric( cut(v4742,
                                          breaks = Hmisc::wtd.quantile(x = v4742
                                                                       , weights = V00291
                                                                       , probs = seq(0, 1, by=0.2), na.rm=T)
                                          , include.lowest= TRUE, labels=1:5)), by = metro]
#pns2019[, quintileMetro := function_ranks(x = v4742,w = V00291
#                                               ,p = seq(0, 1, by=0.2)
#                                               ,label = 1:5), by = metro]

# function to Create Quartile for different Metro Areas
pns2019[, quartileMetro:= as.numeric( cut(v4742
                                          , breaks = Hmisc::wtd.quantile(x = v4742
                                                                         , weights = V00291
                                                                         , probs = seq(0, 1, by=0.25), na.rm=T),
                                          include.lowest= TRUE, labels=1:4)), by = metro]
#pns2019[, quartileMetro := function_ranks(x = v4742,w = V00291
#                                          ,p = seq(0, 1, by=0.25)
#                                          ,label = 1:4), by = metro]


# number of cases in each Region/Metro area by income quantile
#Numero de casos dentro de cada Decil tem que ser igual/proximo
table(pns2019$quintileRegion, pns2019$region)
table(pns2019$quintileMetro, pns2019$metro)
gc(reset = T)
#     


# 3. Export  ----------------
readr::write_rds(x = pns2019
                 ,file =  "../../data/transporte_ativo_2008-2019/pns2019_dt.rds"
                 , compress = "gz")

