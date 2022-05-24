# This script downloads PNS survey data of 2013 from IBGE website and saves it to your local computer
# Script written by Rafael Pereira (urbandemographics.blogspot.com) and modified 
# by Joao Pedro Bazzo
# Jun. 2020, Brasilia


# 1. Load packages -------------------------------------------------------
rm(list=ls())
gc(reset=TRUE)
library(PNADcIBGE) # devtools::install_github("Gabriel-Assuncao/PNADcIBGE")
library(survey)
library(srvyr)
library(data.table)
library(magrittr)
library(microdadosBrasil) #
library(SAScii)
source("R/0.1_parse_sasci.R")
# Household data - Read .txt data using SAS instructions
# -
# SAS instructions

sas_path_2013 <- "../../data-raw/PNS/2013/input_PNS_2013.sas"

sas_file <- parse.sasci_mod(sas_ri = sas_path_2013,beginline = 1)

# Read the .txt file

txt_path_2013 <- "../../data-raw/PNS/2013/PNS_2013.txt"
pns2013 <-   readr::read_fwf(file = txt_path_2013,
                                col_positions = readr::fwf_widths(widths = dput(sas_file$width),
                                                                  col_names = (dput(sas_file$varname))),
                                progress = interactive())

data.table::setDT(pns2013)



# Indicate which columns will be read from .txt files
myvaribles <- c(
  "V0001"      # state
  , "C006"      # sex
  , "C009"      # race
  , "C008"      # age
  , "VDD004"    # Educational attainment
  , "P040"      # Active commute
  , "P04101"    # Active commute time (hours)
  , "P04102"    # Active commute time (minutes)
  , "P04301"    # active travel to habitual activities
  , "P04302"    # active travel time to habitual activities
  , "P00101"    # Weight
  , "P00401"    # Height
  , "N001"      # health perception
  , "V0025"     # person selected for long questionaire
  , "M001"      # Type of interview
  , "UPA_PNS"   # UPA
  , "V0024"     # Strata
  , "V0029"     # person sample weight without calibratio
  , "V00291"    # person sample weight with calibration
  , "V00292"    # Population projection
  , "V00283"    # Dominio de pos-estrato 1
  , "V00293"    # Dominio de pos-estrato 2
  , "C004"      # Condi??o no domic?lio
  , "V0006_PNS" # N?mero de ordem do domic?lio na PNS
  , "E01602"   # Income
  , "E01604"    # Income
  , "E01802"    # Income
  , "E01804"    # Income
  , "F00102"    # Income
  , "F00702"    # Income
  , "F00802"    # Income
  , "VDF00102"    # Income
)


pns2013[,(myvaribles):= lapply(.SD, as.numeric), .SDcols = myvaribles]
# 2. Rename variables -----
## 2.1 Socioeconomic  ----------------

# Urban vs Rural areas
# v0026 - Tipo de situacao censitaria
pns2013[V0026 == 1, urban := "Urbana"]
pns2013[V0026 == 2, urban := "Rural"]
table(pns2013$V0026)
table(pns2013$urban,exclude = FALSE)

# Vehicle ownership Variable, make it compatible with PNAD
# A01817 - Neste domicilio existe motocicleta? ( 1 = sim, 2= nao)
# A020 - Quantos carros tem este domicílio? (0 = Nenhum)
pns2013[A01817 == 2 & A020 > 0 , vehicleOwnership := "Carro"] #  2
pns2013[A01817 == 1 & A020 == 0, vehicleOwnership := "Motocicleta"] #  4 
pns2013[A01817 == 1 & A020 > 0 , vehicleOwnership := "Carro + Motocicleta"] # 6 
pns2013[A01817 == 2 & A020 == 0, vehicleOwnership := "Nenhum"] # 8
table(pns2013$vehicleOwnership,exclude = FALSE)

# Dummy for Vehicle ownership Variable, make it compatible with PNAD
# If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit
pns2013[, dummyVehicle := data.table::fifelse(vehicleOwnership == "Nenhum", 0, 1)]

pns2013[,dummyVehicle := factor(dummyVehicle, levels = c(1,0),
                                   labels = c("Sim","Não"))]

table(pns2013$dummyVehicle,exclude = FALSE)
# V0029	 | Peso do morador selecionado sem calibracao
# V00291 | Peso do morador selecionado com calibracao
# V00292 | Projecao da populacao para moradores selecionados
# V00283 | Domínio de projeção para domicílio e moradores
changeCols <- c("V0029",'V00291','V00292','V00293')
pns2013[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]

# year variable     
data.table::setDT(pns2013)[, year := 2013]

# Create  age groups with 5y intervals
# C008	C8	Idade do morador na data de referência
pns2013[C008 >= 00 & C008<05, agegroup := "0-4"]
pns2013[C008 >= 04 & C008<14, agegroup := "5-13"]
pns2013[C008 >= 13 & C008<18, agegroup := "14-17"]
pns2013[C008 >= 17 & C008<25, agegroup := "18-24"]
pns2013[C008 >= 24 & C008<30, agegroup := "25-29"]
pns2013[C008 >= 29 & C008<35, agegroup := "30-34"]
pns2013[C008 >= 34 & C008<40, agegroup := "35-39"]
pns2013[C008 >= 39 & C008<45, agegroup := "40-44"]
pns2013[C008 >= 44 & C008<50, agegroup := "45-49"]
pns2013[C008 >= 49 & C008<55, agegroup := "50-54"]
pns2013[C008 >= 54 & C008<60, agegroup := "55-59"]
pns2013[C008 >= 59 & C008<65, agegroup := "60-64"]
pns2013[C008 >= 64, agegroup :="65+"]

pns2013[C008 >= 00 & C008<05, agegroup1 := "0-4"]
pns2013[C008 >= 04 & C008<14, agegroup1 := "5-13"]
pns2013[C008 >= 13 & C008<18, agegroup1 := "14-17"]
pns2013[C008 >= 18 & C008<20, agegroup1 := "18-19"]
pns2013[C008 >= 20 & C008<25, agegroup1 := "20-24"]
pns2013[C008 >= 24 & C008<30, agegroup1 := "25-29"]
pns2013[C008 >= 29 & C008<35, agegroup1 := "30-34"]
pns2013[C008 >= 34 & C008<40, agegroup1 := "35-39"]
pns2013[C008 >= 39 & C008<45, agegroup1 := "40-44"]
pns2013[C008 >= 44 & C008<50, agegroup1 := "45-49"]
pns2013[C008 >= 49 & C008<55, agegroup1 := "50-54"]
pns2013[C008 >= 54 & C008<60, agegroup1 := "55-59"]
pns2013[C008 >= 59 & C008<65, agegroup1 := "60-64"]
pns2013[C008 >= 64, agegroup :="65+"]
table(pns2013$agegroup,exclude = FALSE)  

# Create age groups with bigger age interval
pns2013[C008 >=0 & C008<18, AGE :="0-17"]
pns2013[C008 >17 & C008<25, AGE :="18-24"]
pns2013[C008 >=25 & C008<35, AGE :="25-34"]
pns2013[C008 >=35 & C008<45, AGE :="35-44"]
pns2013[C008 >=45 & C008<55, AGE :="45-54"]
pns2013[C008 >=55 & C008<65, AGE :="55-64"]
pns2013[C008 >=65,  AGE :="65+"]

table(pns2013$AGE,exclude = FALSE)  


# Create BMI  - Body Max Index (weight / height)
# P00101	P1	Se sim, qual o peso (kg)?
# P00401	P4	Especifique a altura em cm (P004)

pns2013[,P00101 := as.numeric(P00101)] # weight
pns2013[,P00401 := as.numeric(P00401)] # height
summary(pns2013$P00101) # weight
summary(pns2013$P00401) # height
pns2013[, P00401 := ifelse(P00401==0 , NA, P00401/100)] # If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit

#compute BMI    
pns2013[, bmi := P00101 / P00401^2 ]
summary(pns2013$bmi)
#plot(pns2013$bmi)

# Recode Education Variable, make it compatible with PNAD
# VDD004		Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade) -  SISTEMA DE 8 ANOS
pns2013[VDD004 == 1, edugroup_large := "Sem instrução"]
pns2013[VDD004 == 2, edugroup_large := "Fundamental incompleto ou equivalente"]
pns2013[VDD004 == 3, edugroup_large := "Fundamental completo ou equivalente"]
pns2013[VDD004 == 4, edugroup_large := "Médio incompleto ou equivalente"]
pns2013[VDD004 == 5, edugroup_large := "Médio completo ou equivalente"]
pns2013[VDD004 == 6, edugroup_large := "Superior incompleto ou equivalente"]
pns2013[VDD004 == 7, edugroup_large := "Superior completo"]

table(pns2013$edugroup_large,exclude = FALSE)

# Educational groups
pns2013[VDD004 < 3               , edugroup := "Sem instrução + Fundamental incompleto"]
pns2013[VDD004 == 3 | VDD004 == 4, edugroup := "Fundamental completo"]
pns2013[VDD004 == 5 | VDD004 == 6, edugroup := "Médio completo"]
pns2013[VDD004 == 7              , edugroup := "Superior completo"]

table(pns2013$edugroup,exclude = FALSE)

# Recode Race variable into string
# C009	C9	Cor ou raça

unique(pns2013$C009)
pns2013[,C009 := as.character(C009)]
pns2013[C009 == "1", raca := "Branca"]
pns2013[C009 == "2", raca := "Preta"]
pns2013[C009 == "3", raca := "Amarela"]
pns2013[C009 == "4", raca := "Parda"]
pns2013[C009 == "5", raca := "Indígena"]
pns2013[C009 == "9", raca :=  NA]
table(pns2013$raca,exclude = FALSE)
table(pns2013$C009,exclude = FALSE)

# Recode Sex Variable, make it compatible with PNAD
# C006	C6	Sexo
pns2013[C006 == 1, sexo := "Masculino"]
pns2013[C006 == 2, sexo := "Feminino"]
table(pns2013$sexo,exclude = FALSE)

## 2.2 Region -----

# Recode Urban Rural Variable, make it compatible with PNAD
# V0026		Tipo de situação censitária
pns2013[V0026 == 1 , urban := "Urbano"]
pns2013[V0026 == 2 , urban := "Rural"]
table(pns2013$V0026,exclude = FALSE)
table(pns2013$urban,exclude = FALSE)

# cria VariaVel de Regiao
# V0001	     	Unidade da Federação
pns2013[V0001 < 20, region :="Norte"]
pns2013[V0001 > 19 & V0001 < 30, region :="Nordeste"]
pns2013[V0001 > 29 & V0001 < 40, region :="Sudeste"]
pns2013[V0001 > 39 & V0001 < 50, region :="Sul"]
pns2013[V0001 > 49 & V0001 < 60, region :="Centro-Oeste"]
table(pns2013$region,exclude = FALSE)    

# V0001		Unidade da Federação
pns2013[, V0001 := as.numeric(V0001)]
pns2013[V0001 == 11, uf := "RO"]
pns2013[V0001 == 12, uf := "AC"]
pns2013[V0001 == 13, uf := "AM"]
pns2013[V0001 == 14, uf := "RR"]
pns2013[V0001 == 15, uf := "PA"]
pns2013[V0001 == 16, uf := "AP"]
pns2013[V0001 == 17, uf := "TO"]
pns2013[V0001 == 21, uf := "MA"]
pns2013[V0001 == 22, uf := "PI"]
pns2013[V0001 == 23, uf := "CE"]
pns2013[V0001 == 24, uf := "RN"]
pns2013[V0001 == 25, uf := "PB"]
pns2013[V0001 == 26, uf := "PE"]
pns2013[V0001 == 27, uf := "AL"]
pns2013[V0001 == 28, uf := "AL"]
pns2013[V0001 == 29, uf := "BA"]
pns2013[V0001 == 31, uf := "MG"]
pns2013[V0001 == 32, uf := "ES"]
pns2013[V0001 == 33, uf := "RJ"]
pns2013[V0001 == 35, uf := "SP"]
pns2013[V0001 == 41, uf := "PR"]
pns2013[V0001 == 42, uf := "SC"]
pns2013[V0001 == 43, uf := "RS"]
pns2013[V0001 == 50, uf := "MS"]
pns2013[V0001 == 51, uf := "MT"]
pns2013[V0001 == 52, uf := "GO"]
pns2013[V0001 == 53, uf := "DF"]

table(pns2013$uf,exclude = FALSE)

pns2013[V0001 == 11, uf_name :="Rondonia"]
pns2013[V0001 == 12, uf_name :="Acre"]
pns2013[V0001 == 13, uf_name :="Amazonas"]
pns2013[V0001 == 14, uf_name :="Roraima"]
pns2013[V0001 == 15, uf_name :="Para"]
pns2013[V0001 == 16, uf_name :="Amapa"]
pns2013[V0001 == 17, uf_name :="Tocantins"]
pns2013[V0001 == 21, uf_name :="Maranhao"]
pns2013[V0001 == 22, uf_name :="Piaui"]
pns2013[V0001 == 23, uf_name :="Ceara"]
pns2013[V0001 == 24, uf_name :="Rio Grande do Norte"]
pns2013[V0001 == 25, uf_name :="Paraiba"]
pns2013[V0001 == 26, uf_name :="Pernambuco"]
pns2013[V0001 == 27, uf_name :="Alagoas"]
pns2013[V0001 == 28, uf_name :="Sergipe"]
pns2013[V0001 == 29, uf_name :="Bahia"]
pns2013[V0001 == 31, uf_name :="Minas Gerais"]
pns2013[V0001 == 32, uf_name :="Espirito Santo"]
pns2013[V0001 == 33, uf_name :="Rio de Janeiro"]
pns2013[V0001 == 35, uf_name :="São Paulo"]
pns2013[V0001 == 41, uf_name :="Parana"]
pns2013[V0001 == 42, uf_name :="Santa Catarina"]
pns2013[V0001 == 43, uf_name :="Rio Grande do Sul"]
pns2013[V0001 == 50, uf_name :="Mato Grosso do Sul"]
pns2013[V0001 == 51, uf_name :="Mato Grosso"]
pns2013[V0001 == 52, uf_name :="Goias"]
pns2013[V0001 == 53, uf_name :="Federal District"]

table(pns2013$uf_name,exclude = FALSE)

# Recode Metropolitan area Variable, make it compatible with PNAD
# V0031		Tipo de área	
# 1	Capital
# 2	Resto da RM (Região Metropolitana, excluindo a capital)
# 3	RIDE (excluindo a capital)
# 4	Resto da UF (Unidade da Federação, excluindo a região metropolitana e RIDE)
pns2013[V0031 == 1 | V0031 == 2, v4727 := 1]
pns2013[V0031 > 2, v4727 := 3]
table(pns2013$v4727,exclude = FALSE)

# Create Variable Metropolitan area
# V0031		Tipo de área	
pns2013[V0031 == 4, metro := "Restante das UF"]
pns2013[V0001 == 15 & V0031 < 3, metro := "Belém"]
pns2013[V0001 == 23 & V0031 < 3, metro := "Fortaleza"]
pns2013[V0001 == 26 & V0031 < 3, metro := "Recife"]
pns2013[V0001 == 29 & V0031 < 3, metro := "Salvador"]
pns2013[V0001 == 31 & V0031 < 3, metro := "Belo Horizonte"]
pns2013[V0001 == 33 & V0031 < 3, metro := "Rio de Janeiro"]
pns2013[V0001 == 35 & V0031 < 3, metro := "São Paulo"]
pns2013[V0001 == 41 & V0031 < 3, metro := "Curitiba"]
pns2013[V0001 == 43 & V0031 < 3, metro := "Porto Alegre"]
pns2013[V0001 == 53 & V0031 < 3, metro := "Distrito Federal"]

table(pns2013$metro,exclude = FALSE)

pns2013[,country := "Brasil"]
pns2013[,dummyMetro := fifelse(is.na(metro),"Non-metro","Metro")]
gc(reset = T)

## 2.3 Mobility -----
### create indicator variable of ind. above 18yearsold that practice active travel for > 30minutes
## this is the definition used in table 3.4.1.1 of IBGE report
#
# P04101	Quantas horas o(a) Sr(a) gasta para percorrer este trajeto a pé ou de bicicleta, 
# considerando a ida e a volta do trabalho?
# P04102	Quantos minutos o(a) Sr(a) gasta para percorrer este trajeto a pé ou de bicicleta,
# considerando a ida e a volta do trabalho?
# P04301	No dia que o(a) Sr(a) faz esta atividade, quantas horas o(a) Sr(a) gasta no 
# deslocamento a pé ou de   bicicleta, considerando a ida e a volta?
# P04302	P43	No dia que o(a) Sr(a) faz esta atividade, quantos minutos o(a) Sr(a) gasta
# no deslocamento a pé ou de   bicicleta, considerando a ida e a volta?


pns2013[,P04101 := as.numeric(P04101)]
pns2013[,P04102 := as.numeric(P04102)]
pns2013[,P04301 := as.numeric(P04301)]
pns2013[,P04302 := as.numeric(P04302)]
pns2013[, actv_commutetime := P04101 * 60 + P04102] # Active commute time

summary(pns2013$actv_commutetime)
# making it compatible with PNAD2008
pns2013[,actv_commutetime_00to09 := fifelse(actv_commutetime >= 01 & actv_commutetime <= 09,1,0)] # Menos de 10 minutos
pns2013[,actv_commutetime_10to19 := fifelse(actv_commutetime >= 10 & actv_commutetime <= 19,1,0)] # 10 a 19 minutos
pns2013[,actv_commutetime_20to29 := fifelse(actv_commutetime >= 20 & actv_commutetime <= 29,1,0)] # 20 a 29 minutos
pns2013[,actv_commutetime_30to44 := fifelse(actv_commutetime >= 30 & actv_commutetime <= 44,1,0)] # 30 a 44 minutos
pns2013[,actv_commutetime_45to59 := fifelse(actv_commutetime >= 45 & actv_commutetime <= 59,1,0)] # 45 a 59 minutos
pns2013[,actv_commutetime_from60 := fifelse(actv_commutetime >= 60,1,0)] # 60 minutos ou mais 
pns2013[,actv_commutetime_from30 := fifelse(actv_commutetime >= 30,1,0)] # 60 minutos ou mais 

table(pns2013$P040,exclude = FALSE)
table(pns2013$actv_commutetime_00to09,exclude = FALSE)
table(pns2013$actv_commutetime_10to19,exclude = FALSE)
table(pns2013$actv_commutetime_20to29,exclude = FALSE)
table(pns2013$actv_commutetime_30to44,exclude = FALSE)
table(pns2013$actv_commutetime_45to59,exclude = FALSE)
table(pns2013$actv_commutetime_from60,exclude = FALSE)
table(pns2013$actv_commutetime_from30,exclude = FALSE)

# physicallyactive15 / 30
pns2013[,physicallyactive15 := fifelse(actv_commutetime >= 15,1,0)] # 15 minutos ou mais 
pns2013[,physicallyactive30 := fifelse(actv_commutetime >= 30,1,0)] # 30 minutos ou mais 

# Recode Acctive Travel Variable P040 into string
pns2013[, P040 := as.character(P040)]
pns2013[P040 == 1, P040 := "Sim, todo o trajeto"]
pns2013[P040 == 2, P040 := "Sim, parte do trajeto"]
pns2013[P040 == 3, P040 :=  "Não"]
table(pns2013$P040,exclude = FALSE)
# Recode Active Travel Variable, make it compatible with PNAD
# P040	-	Para ir ou voltar do trabalho, o(a) Sr(a) faz algum trajeto a pé ou de bicicleta
# 1	Sim, todo o trajeto
# 2	Sim, parte do trajeto
# 3	Não
# Não aplicável

pns2013[P040 %like% "Sim", v1410 := "Sim"]
pns2013[P040 == "Não", v1410 := "Não"]
table(pns2013$v1410,exclude = FALSE)
table(pns2013$P040,exclude = FALSE)


## 2.4 Income ----

# E01602	E16	Qual era o rendimento bruto mensal ou retirada que ________________ fazia normalmente nesse trabalho? - Valor em dinheiro (R$)
# E01604	E16	Qual era o rendimento bruto mensal ou retirada que ________________ fazia normalmente nesse trabalho? - Valor estimado dos produtos ou mercadorias (R$)
# E01802	E18	Qual era o rendimento bruto mensal ou retirada que ________________ fazia normalmente nesse(s) outro(s) trabalho(s)? - Valor em dinheiro (R$)
# E01804	E18	Qual era o rendimento bruto mensal ou retirada que ________________ fazia normalmente nesse(s) outro(s) trabalho(s)? - Valor estimado dos produtos ou mercadorias (R$)
# F00102	F1	Valor recebido em reais (F001)
# F00702	F7	Valor recebido em reais (F007)
# F00802	F8	Valor recebido em reais (F008)
# VDF00102	F8	Valor recebido em reais (VDF001)

colnames_income <-  c('E01602', 'E01604', 'E01802', 'E01804',
                      'F00102', 'F00702', 'F00802', 'VDF00102')
pns2013[,(colnames_income) := lapply(.SD,as.numeric)
           , .SDcols = colnames_income]

# to numeric
# VDC001		Número de componentes do domicílio 
# (exclusive as pessoas cuja condição na família era pensionista,
# empregado doméstico ou parente do empregado doméstico) 


pns2013[,VDC001 := as.numeric(VDC001)]
summary(pns2013$VDC001)


# Summary of income variables

summary(pns2013$E01602)
summary(pns2013$E01604)
summary(pns2013$E01802)
summary(pns2013$E01804)
summary(pns2013$F00102)
summary(pns2013$F00702)
summary(pns2013$F00802)
summary(pns2013$VDF00102)

#  Household Income per Capita, compatible with PNAD 2008 data
# C004	C4	Condição no domicílio:

pns2013[ C004 <17 , v4742 := sum( E01602, E01604, E01802,
                                     E01804, F00102, F00702, 
                                     F00802, VDF00102, na.rm = T) / VDC001,
            by= .(V0001, V0024, UPA_PNS, V0006_PNS)] # sum all income sources


summary(pns2013$v4742)
# joao | # >Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# joao | # >0.0    300.0    552.8    973.8   1000.0 145769.0    17063 
# rafa | # >Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# rafa | # >  0     340      670    1140    1190  146000  130415 
# 
# # 1119 casos com RDPC igual a 0
head(table(pns2013$v4742))


# Create income quantiles
# V0029		Peso do morador selecionado sem calibração	5 dígitos e 8 casas decimais	
# Peso do morador selecionado com correção de não entrevista sem calibração pela projeção 
# de população para morador selecionado
# 
# V00282		Projeção da população		Projeção da população
# 
# V00291		Peso do morador selecionado com calibração	5 dígitos e 8 casas decimais	
# Peso do morador selecionado com correção de não entrevista com calibração pela 
# projeção de população para morador selecionado - Usado no cálculo de indicadores 
# de morador selecionado

# Create  var. income deciles of Monthly household income per capita
pns2013[, decileBR:= as.numeric( cut(v4742
                                        , breaks = Hmisc::wtd.quantile(x = v4742
                                                                       , weights =  V00291
                                                                       , probs = seq(0, 1, by=0.1)
                                                                       , na.rm=T),
                                        include.lowest = TRUE, labels = 1:10))]

# Checking Table
table(pns2013$decileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo


# Create  var. income quintile of Monthly household income per capitade
pns2013[, quintileBR:= as.numeric( cut(v4742
                                          , breaks = Hmisc::wtd.quantile(x = v4742
                                                                         , weights = V00291
                                                                         ,probs=seq(0, 1, by=0.2), na.rm=T),
                                          include.lowest= TRUE, labels=1:5))]

table(pns2013$quintileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo

# function to Create Quintile for different regions
pns2013[, quintileRegion:= as.numeric( cut(v4742
                                              , breaks = Hmisc::wtd.quantile(x = v4742
                                                                             , weights = V00291
                                                                             ,probs=seq(0, 1, by=0.2), na.rm=T),
                                              include.lowest= TRUE, labels=1:5)), by = region]


# function to Create Quartile for different regions
pns2013[, quartileRegion:= as.numeric( cut(v4742
                                              , breaks = Hmisc::wtd.quantile(x = v4742
                                                                             , weights = V00291
                                                                             ,probs=seq(0, 1, by=0.25), na.rm=T),
                                              include.lowest= TRUE, labels=1:4)), by = region]


# function to Create Quintile for different Metro Areas
pns2013[, quintileMetro:= as.numeric( cut(v4742,
                                             breaks = Hmisc::wtd.quantile(x = v4742
                                                                          , weights = V00291
                                                                          , probs = seq(0, 1, by=0.2), na.rm=T)
                                             , include.lowest= TRUE, labels=1:5)), by = metro]

# function to Create Quartile for different Metro Areas
pns2013[, quartileMetro:= as.numeric( cut(v4742
                                             , breaks = Hmisc::wtd.quantile(x = v4742
                                                                            , weights = V00291
                                                                            , probs = seq(0, 1, by=0.25), na.rm=T),
                                             include.lowest= TRUE, labels=1:4)), by = metro]



# number of cases in each Region/Metro area by income quantile
#Numero de casos dentro de cada Decil tem que ser igual/proximo
table(pns2013$quintileRegion, pns2013$region)
table(pns2013$quintileMetro, pns2013$metro)
gc(reset = T)


# 3. Save modified DAta files  ----------------

dir.create("data")
dir.create("data/PNS")
readr::write_rds(x = pns2013,file =  "../../data/transporte_ativo_2008-2019/pns2013.rds"
                 , compress = "gz")


