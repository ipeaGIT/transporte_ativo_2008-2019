# This script downloads PNS survey data of 2013 from IBGE website and saves it to your local computer
# Script written by Rafael Pereira (urbandemographics.blogspot.com) and modified 
# by Joao Pedro Bazzo
# Jun. 2020, Brasilia


##################### Load packages -------------------------------------------------------
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
pns2013dom <-   readr::read_fwf(file = txt_path_2013,
                                col_positions = readr::fwf_widths(widths = dput(sas_file$width),
                                                           col_names=(dput(sas_file$varname))),
                                progress = interactive())


# make sure all variables are 'numeric' class
data.table::setDT(pns2013dom)
changeCols <- colnames(pns2013dom)[1:69]
pns2013dom[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]

# clean memory
gc(reset = T)


## Indicate which columns will be read from .txt files
myvariblesPES <- c(
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
  , "O009"      # car accident
  , "O011"      # travel mode when injured
  , "O014"      # accident hindered habitual activities
  , "O020"      # any sequel and / or disability due to this traffic accident
  , "Q002"      # Ever diagnosed with hypertension
  , "Q003"      # age at diagnosis for hypertension
  , "Q030"      # Ever diagnosed with diabetes
  , "Q031"      # age at diagnosis for diabetes
  , "Q060"      # Ever diagnosed with high cholesterol
  , "Q061"      # age at diagnosis for high cholesterol
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


########## 2. Recode Household data  ----------------

# Urban vs Rural areas
# v0026 - Tipo de situacao censitaria
pns2013dom[V0026 == 1, urban := "Urbana"]
pns2013dom[V0026 == 2, urban := "Rural"]

# Vehicle ownership Variable, make it compatible with PNAD
# A01817 - Neste domicilio existe motocicleta? ( 1 = sim, 2= nao)
# A020 - Quantos carros tem este domicílio? (0 = Nenhum)
pns2013dom[A01817 == 2 & A020 > 0 , v2032 := "Carro"] #  2
pns2013dom[A01817 == 1 & A020 == 0, v2032 := "Motocicleta"] #  4 
pns2013dom[A01817 == 1 & A020 > 0 , v2032 := "Carro + Motocicleta"] # 6 
pns2013dom[A01817 == 2 & A020 == 0, v2032 := "Nenhum"] # 8

# Dummy for Vehicle ownership Variable, make it compatible with PNAD
# If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit
pns2013dom[, dummyVehicle := data.table::fifelse(v2032 == "Nenhum", 0, 1)]

pns2013dom[,dummyVehicle := factor(dummyVehicle, levels = c(1,0),
                                  labels = c("Sim","Não"))]

# 3. Merge household and individual data sets---------

# Merge datasets
# V0029	 | Peso do morador selecionado sem calibracao
# V00291 | Peso do morador selecionado com calibracao
# V00292 | Projecao da populacao para moradores selecionados
# V00283 | Domínio de projeção para domicílio e moradores


changeCols1 <- c("V0029",'V00291','V00292','V00293')
pns2013dom[,(changeCols1):= lapply(.SD, as.numeric), .SDcols = changeCols1]


# clean memory
#rm(list=setdiff(ls(), c("pns2013", "pns2013dom")))
gc(reset = T)





# 4: Add Variables to pns2013 data ---------------------------------

## household ID ( unnecessary)
# V0001	     	Unidade da Federação
# V0024		    Estrato
# UPA_PNS		  UPA
# V0006_PNS		Número de ordem do domicílio na PNS

pns2013dom[, householdID := paste(V0001, V0024, UPA_PNS, V0006_PNS, sep = ".") ]

# year variable     
data.table::setDT(pns2013dom)[, year := 2013]

# # Count variable     
# pns2013dom[, vcount := 1]
# table(pns2013$vcount)

# cria VariaVel de Regiao
# V0001	     	Unidade da Federação
pns2013dom[V0001 < 20, region :="Norte"]
pns2013dom[V0001 > 19 & V0001 < 30, region :="Nordeste"]
pns2013dom[V0001 > 29 & V0001 < 40, region :="Sudeste"]
pns2013dom[V0001 > 39 & V0001 < 50, region :="Sul"]
pns2013dom[V0001 > 49 & V0001 < 60, region :="Centro-Oeste"]
table(pns2013dom$region,exclude = FALSE)    



# Create age groups with bigger age1 interval
# C008	C8	Idade do morador na data de referência

pns2013dom[C008 >= 0 & C008<15, AGE_group :="0-15"]
pns2013dom[C008 >= 15 & C008<20, AGE_group :="15-20"]
pns2013dom[C008 >= 20 & C008<25, AGE_group :="20-25"]
pns2013dom[C008 >= 25 & C008<30, AGE_group :="25-30"]
pns2013dom[C008 >= 30 & C008<35, AGE_group :="30-35"]
pns2013dom[C008 >= 35 & C008<40, AGE_group :="35-40"]
pns2013dom[C008 >= 40 & C008<45, AGE_group :="40-45"]
pns2013dom[C008 >= 45 & C008<50, AGE_group :="45-50"]
pns2013dom[C008 >= 50 & C008<55, AGE_group :="50-55"]
pns2013dom[C008 >= 55 & C008<60, AGE_group :="55-60"]
pns2013dom[C008 >= 60 & C008<65, AGE_group :="60-65"]
pns2013dom[C008 >= 65, AGE_group :="65+"]

table(pns2013dom$AGE_group,exclude = FALSE)  

# Create age groups with bigger age interval
pns2013dom[C008 >=0 & C008<18, AGE :="0-17"]
pns2013dom[C008 >17 & C008<25, AGE :="18-24"]
pns2013dom[C008 >=25 & C008<35, AGE :="25-34"]
pns2013dom[C008 >=35 & C008<45, AGE :="35-44"]
pns2013dom[C008 >=45 & C008<55, AGE :="45-54"]
pns2013dom[C008 >=55 & C008<65, AGE :="55-64"]
pns2013dom[C008 >=65,  AGE :="65+"]

table(pns2013dom$AGE,exclude = FALSE)  

# Create  age groups with 5y intervals
pns2013dom[C008 <5, agegroup := "0-4"]
pns2013dom[C008 >4 & C008 <14, agegroup := "5-13"]
pns2013dom[C008 >13 & C008 <18, agegroup := "14-17"]
pns2013dom[C008 >17 & C008 <25, agegroup := "18-24"]
pns2013dom[C008 >24 & C008 <30, agegroup := "25-29"]
pns2013dom[C008 >29 & C008 <35, agegroup := "30-34"]
pns2013dom[C008 >34 & C008 <40, agegroup := "35-39"]
pns2013dom[C008 >39 & C008 <45, agegroup := "40-44"]
pns2013dom[C008 >44 & C008 <50, agegroup := "45-49"]
pns2013dom[C008 >49 & C008 <55, agegroup := "50-54"]
pns2013dom[C008 >54 & C008 <60, agegroup := "55-59"]
pns2013dom[C008 >59 & C008 <65, agegroup := "60-64"]
pns2013dom[C008 >64, agegroup := "65+"]

table(pns2013dom$agegroup,exclude = FALSE)


# Create BMI  - Body Max Index (weight / height)
# P00101	P1	Se sim, qual o peso (kg)?
# P00401	P4	Especifique a altura em cm (P004)

pns2013dom[,P00101 := as.numeric(P00101)] # weight
pns2013dom[,P00401 := as.numeric(P00401)] # height
summary(pns2013dom$P00101) # weight
summary(pns2013dom$P00401) # height
pns2013dom[, P00401 := ifelse(P00401==0 , NA, P00401/100)] # If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit

#compute BMI    
pns2013dom[, bmi := P00101 / P00401^2 ]
summary(pns2013dom$bmi)
plot(pns2013dom$bmi)

# Recode Education Variable, make it compatible with PNAD
# VDD004		Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade) -  SISTEMA DE 8 ANOS
pns2013dom[VDD004 == 1, v4745 := "Sem instrução"]
pns2013dom[VDD004 == 2, v4745 := "Fundamental incompleto ou equivalente"]
pns2013dom[VDD004 == 3, v4745 := "Fundamental completo ou equivalente"]
pns2013dom[VDD004 == 4, v4745 := "Médio incompleto ou equivalente"]
pns2013dom[VDD004 == 5, v4745 := "Médio completo ou equivalente"]
pns2013dom[VDD004 == 6, v4745 := "Superior incompleto ou equivalente"]
pns2013dom[VDD004 == 7, v4745 := "Superior completo"]

table(pns2013dom$v4745,exclude = FALSE)
is.factor(pns2013dom$v4745)
levels(pns2013dom$v4745)

pns2013dom[, v4745 := factor(x = v4745
                             , levels = c("Uneducated", "Incomplete primary school", 
                                          "Complete primary school", "Incomplete high school",
                                          "Complete high school", "Incomplete university degree",
                                          "University degree")
                             , labels = c("Sem instrução","Fundamental incompleto ou equivalente",
                                          "Fundamental completo ou equivalente","Médio incompleto ou equivalente",
                                          "Médio completo ou equivalente","Superior incompleto ou equivalente",
                                          "Superior completo"))]

# Educational groups
pns2013dom[VDD004 < 3, edugroup := "Sem instrução + Fundamental incompleto"]
pns2013dom[VDD004 == 3 | VDD004 == 4, edugroup := "Fundamental completo"]
pns2013dom[VDD004 == 5 | VDD004 == 6, edugroup := "Médio completo"]
pns2013dom[VDD004 == 7, edugroup := "Superior completo"]

table(pns2013dom$edugroup,exclude = FALSE)

# Recode Race variable into string
# C009	C9	Cor ou raça

pns2013dom[,C009 := as.character(C009)]
pns2013dom[C009 == 1, C009 := "Branca"]
pns2013dom[C009 == 2, C009 := "Preta"]
pns2013dom[C009 == 3, C009 := "Amarela"]
pns2013dom[C009 == 4, C009 := "Parda"]
pns2013dom[C009 == 5, C009 := "Indígena"]
pns2013dom[C009 == 9, C009 :=  NA]
table(pns2013dom$C009,exclude = FALSE)

# Recode UF Variable----------------
# V0001		Unidade da Federação

pns2013dom[, V0001 := as.numeric(V0001)]
pns2013dom[V0001 == 11, uf :=	"RO"]
pns2013dom[V0001 == 12, uf :=	"AC"]
pns2013dom[V0001 == 13, uf :=	"AM"]
pns2013dom[V0001 == 14, uf :=	"RR"]
pns2013dom[V0001 == 15, uf :=	"PA"]
pns2013dom[V0001 == 16, uf :=	"AP"]
pns2013dom[V0001 == 17, uf :=	"TO"]
pns2013dom[V0001 == 21, uf :=	"MA"]
pns2013dom[V0001 == 22, uf :=	"PI"]
pns2013dom[V0001 == 23, uf :=	"CE"]
pns2013dom[V0001 == 24, uf :=	"RN"]
pns2013dom[V0001 == 25, uf :=	"PB"]
pns2013dom[V0001 == 26, uf :=	"PE"]
pns2013dom[V0001 == 27, uf :=	"AL"]
pns2013dom[V0001 == 28, uf :=	"AL"]
pns2013dom[V0001 == 29, uf :=	"BA"]
pns2013dom[V0001 == 31, uf :=	"MG"]
pns2013dom[V0001 == 32, uf :=	"ES"]
pns2013dom[V0001 == 33, uf :=	"RJ"]
pns2013dom[V0001 == 35, uf :=	"SP"]
pns2013dom[V0001 == 41, uf :=	"PR"]
pns2013dom[V0001 == 42, uf :=	"SC"]
pns2013dom[V0001 == 43, uf :=	"RS"]
pns2013dom[V0001 == 50, uf :=	"MS"]
pns2013dom[V0001 == 51, uf :=	"MT"]
pns2013dom[V0001 == 52, uf :=	"GO"]
pns2013dom[V0001 == 53, uf :=	"DF"]

table(pns2013dom$uf,exclude = FALSE)

# Recode Sex Variable, make it compatible with PNAD
# C006	C6	Sexo
pns2013dom[C006 == 1, v0302 := "Masculino"]
pns2013dom[C006 == 2, v0302 := "Feminino"]
table(pns2013dom$v0302,exclude = FALSE)

# Recode Urban Rural Variable, make it compatible with PNAD
# V0026		Tipo de situação censitária
pns2013dom[V0026 == 1 , urban := "Urbano"]
pns2013dom[V0026 == 2 , urban := "Rural"]


# Recode Metropolitan area Variable, make it compatible with PNAD
# V0031		Tipo de área	
# 1	Capital
# 2	Resto da RM (Região Metropolitana, excluindo a capital)
# 3	RIDE (excluindo a capital)
# 4	Resto da UF (Unidade da Federação, excluindo a região metropolitana e RIDE)
pns2013dom[V0031 == 1 | V0031 == 2, v4727 := 1]
pns2013dom[V0031>2, v4727 := 3]

9999999999
# Create Variable Metropolitan area
pns2013dom[V0031 == 4, metro := "Restante das UF"]
pns2013dom[V0001 == 15 & V0031<3, metro := "Belém"]
pns2013dom[V0001 == 23 & V0031<3, metro := "Fortaleza"]
pns2013dom[V0001 == 26 & V0031<3, metro := "Recife"]
pns2013dom[V0001 == 29 & V0031<3, metro := "Salvador"]
pns2013dom[V0001 == 31 & V0031<3, metro := "Belo Horizonte"]
pns2013dom[V0001 == 33 & V0031<3, metro := "Rio de Janeiro"]
pns2013dom[V0001 == 35 & V0031<3, metro := "São Paulo"]
pns2013dom[V0001 == 41 & V0031<3, metro := "Curitiba"]
pns2013dom[V0001 == 43 & V0031<3, metro := "Porto Alegre"]
pns2013dom[V0001 == 53 & V0031<3, metro := "Distrito Federal"]




# Recode Active Travel Variable, make it compatible with PNAD
pns2013[P040==1 | P040==2, v1410 := "Sim"]
pns2013[P040==3, v1410 := "Não"]
table(pns2013$v1410)


### create indicator variable of ind. above 18yearsold that practice active travel for > 30minutes
## this is the definition used in table 3.4.1.1 of IBGE report
pns2013[is.na(P04101), P04101 := 0][,P04101 := as.numeric(P04101)]
pns2013[P04102 == ".", P04102 := 0][,P04102 := as.numeric(P04102)]
pns2013[P04301 == ".", P04301 := 0][,P04301 := as.numeric(P04301)]
pns2013[P04302 == ".", P04302 := 0][,P04302 := as.numeric(P04302)]
pns2013[, actv_commutetime := ifelse( is.na(P04101),0, P04101 * 60 + P04102)] # Active commute time
pns2013[, actv_traveltimehabacts := ifelse( is.na(P04301),0, P04301 * 60 + P04302)] #active travel time to habitual activities, such as going to or taking someone to school 
pns2013[, total_actvtraveltime := actv_commutetime + actv_traveltimehabacts] ## total active travel time
pns2013[, physicallyactive30 := ifelse(total_actvtraveltime >= 30,1,0)] # total active travel time >30 (1,0)
pns2013[, physicallyactive15 := ifelse(total_actvtraveltime >= 15,1,0)] # total active travel time >15 (1,0)
pns2013[, actv_commutetime30 := ifelse(actv_commutetime >= 30,1,0)] #commute time >30 (1,0)
pns2013[, actv_commutetime15 := ifelse(actv_commutetime >= 15,1,0)] #commute time >30 (1,0)

table(pns2013$P040)
table(pns2013$actv_commutetime30)


# Recode Acctive Travel Variable P040 into string
pns2013[, P040 := as.character(P040)]
pns2013[P040==1, P040 := "Sim, todo o trajeto"]
pns2013[P040==2, P040 := "Sim, parte do trajeto"]
pns2013[P040==3, P040 :=  "Não"]
table(pns2013$P040)


### 4.1 Income Variables ----


colnames_income <-  c('E01602', 'E01604', 'E01802', 'E01804', 'F00102', 'F00702', 'F00802', 'VDF00102')
pns2013[,(colnames_income) := lapply(.SD,as.numeric), .SDcols = colnames_income]
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
pns2013[ C004 <17 , v4721 := sum( E01602, E01604, E01802, E01804, F00102, F00702, F00802, VDF00102, na.rm = T) / VDC001,
         by= .(V0001, V0024, UPA_PNS, V0006_PNS)] # sum all income sources
summary(pns2013$v4721)


summary(pns2013$v4721)
# # >Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# # >  0     340      670    1140    1190  146000  130415 
# 
# # 1119 casos com RDPC igual a 0
head(table(pns2013$v4721))


########### Create income quantiles

# Create  var. income deciles of Monthly household income per capitade
pns2013[, decileBR:= as.numeric( cut(v4721, breaks=quantile(v4721,
                                                            probs=seq(0, 1, by=0.1), na.rm=T),
                                     include.lowest= TRUE, labels=1:10))]

# Checking Table
table(pns2013$decileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo


# Create  var. income quintile of Monthly household income per capitade
pns2013[, quintileBR:= as.numeric( cut(v4721, breaks=quantile(v4721,
                                                              probs=seq(0, 1, by=0.2), na.rm=T),
                                       include.lowest= TRUE, labels=1:5))]

table(pns2013$quintileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo

# function to Create Quintile for different regions
pns2013[, quintileRegion:= as.numeric( cut(v4721, breaks=quantile(v4721,
                                                                  probs=seq(0, 1, by=0.2), na.rm=T),
                                           include.lowest= TRUE, labels=1:5)), by=region]


# function to Create Quartile for different regions
pns2013[, quartileRegion:= as.numeric( cut(v4721, breaks=quantile(v4721,
                                                                  probs=seq(0, 1, by=0.25), na.rm=T),
                                           include.lowest= TRUE, labels=1:4)), by=region]


# function to Create Quintile for different Metro Areas
pns2013[, quintileMetro:= as.numeric( cut(v4721,
                                          breaks=quantile(v4721, probs=seq(0, 1, by=0.2), na.rm=T),
                                          include.lowest= TRUE, labels=1:5)), by=metro]

# function to Create Quartile for different Metro Areas
pns2013[, quartileMetro:= as.numeric( cut(v4721, breaks=quantile(v4721,
                                                                 probs=seq(0, 1, by=0.25), na.rm=T),
                                          include.lowest= TRUE, labels=1:4)), by=metro]



# number of cases in each Region/Metro area by income quantile
#Numero de casos dentro de cada Decil tem que ser igual/proximo
table(pns2013$quintileRegion, pns2013$region)
table(pns2013$quintileMetro, pns2013$metro)
gc(reset = T)
#     




########## 5. Save modified DAta files  ----------------

dir.create("data")
dir.create("data/PNS")
saveRDS(pns2013, file="./data/PNS/pns2013.Rds")
saveRDS(pns2013dom, file="./data/PNS/pns2013dom.Rds")
saveRDS(pns2013pes, file="./data/PNS/pns2013pes.Rds")




break()

############## TEST RESULTS ##############   ############## TEST RESULTS ############## 
############## TEST RESULTS ##############   ############## TEST RESULTS ############## 
############## TEST RESULTS ##############   ############## TEST RESULTS ############## 


###### 6. Create survey design for PNS 2013 data -----------


# There should be no Missings (NA) in Design Variables
# Count  missing values (NAs)
anyNA(pns2013$V00291)
length(which(is.na(pns2013$V00291)))

# Subset PNS with individuals who answered the detailed questionnaire only
# This eliminates observations with missing values in the weight variable
#PNS2013pesDet <- PNS2013pes[!(is.na(PNS2013pes$V0029))]
pns2013Det <- pns2013[M001==1, ]







#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    
#Cria objeto de desenho da amostra                      
sample.pns13 <- svydesign(data = pns2013Det,
                          id = ~UPA_PNS, #PSU
                          strata = ~V0024, #Strat
                          weights=~V0029, #PesoPessoa: usar peso original
                          nest = TRUE)

## Agora é preciso pós-estratificar:
## A definição dos pós-estratos e os totais populacionais usados são dados por:

## post-estratification of sample design
post_pop <- unique( subset(pns2013Det, select= c(V00293,V00292) ))
names(post_pop)<-c("V00293","Freq")

sample.pns13.pos <- postStratify(sample.pns13, ~V00293, post_pop)

#Subset design population above 18 years old
sample.pns13.18y <- subset(sample.pns13.pos, C008>17)

remove(pns2013, post_pop, sample.pns13); gc()









###### 7. Check PNS13 Results ----------------
# Check against Published Report - ftp://ftp.ibge.gov.br/PNS/2013/pns2013.pdf

#Total population of Brazil above 18 years old (146.3 million)
svytotal (~vcount , sample.pns13.pos)

# Check against Gráfico 1, % of ind. above 18yearsold whith good+very good health (self-assessment)
#ok - Brasil 66.1%
svymean(~factor(N001<3), design= sample.pns13.pos) # 66.1%

#ok - Regioes Nordeste 56.7% , Sul 69.5% , Sudeste 71.5, Norte 59.8%
svyby(~factor(N001<3), ~region, design= sample.pns13.pos,  vartype="ci",  level = 0.95,  svyciprop)


# Tabela 3.4.1.1, % of ind. above 18yearsold that practice active travel for > 30minutes
# NOT so ok - Brasil 31,9% 
print(svyciprop(~physicallyactive30, design=sample.pns13.pos, method = c("likelihood"), level = 0.95))

# Urban 32% vs Rural 31,3%
svyby(~physicallyactive30, ~V0026, design= sample.pns13.pos,  vartype="ci",  level = 0.95,  svyciprop)

# Region
svyby(~physicallyactive30, ~region, design= sample.pns13.pos,  vartype="ci",  level = 0.95,  svyciprop)

# Region and Sex
svyby(~physicallyactive30, ~region+C006, design= sample.pns13.pos,  vartype="ci",  level = 0.95,  svyciprop)


#Race - Gráfico 12 
svyby(~physicallyactive30, ~C009, design= sample.pns13.pos,  vartype="ci",  level = 0.95,  svyciprop)



# Tabela 18.2 - Rendimento mensal médio habitual de todos os trabalhos de 18 anos ou mais de
# idade que possui regime de trabalho não noturno, por sexo, com indicação do intervalo de
# confiança de 95%, segundo as Grandes Regiões - 2013

svyby(~E01602, ~region+v0302, 
      design= subset(sample.pns13.pos, M005==2),  # no night shift
      vartype="ci",  level = 0.95,  na.rm = T, svymean)


beep()
# ============================================================ END