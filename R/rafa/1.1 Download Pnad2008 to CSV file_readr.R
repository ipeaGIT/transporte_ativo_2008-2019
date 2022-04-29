# This scrip downloads PNAD survey data of 2008 from IBGE website and saves to your local computer
# Script written by Rafael Pereira, Aug 2016, Oxford UK.


# Set working directory ------------
setwd("C:/Users/rafa/Desktop/ActiVeTraVel_Pnad-PNS")



###### Load Packages -----------

source("R:/Dropbox/Desloc Ativo2/R scripts/0 LoadPackages.R")





######  1. Donwload Pnad2008 DATA ----------------

# Download and unzip file
  download.file("ftp://ftp.ibge.goV.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/reponderacao_2001_2012/PNAD_reponderado_2008.zip", "PNAD2008.zip", quiet = FALSE)
  dir.create("PNAD2008")
  unzip("PNAD2008.zip", exdir="PNAD2008", junkpaths=T)

  
  
  
  
  
###### 2. Read Pnad2008 DATA ----------------

# Load Data set discription with Variables widths
  load("./PNAD2008/dicPNAD2008.RData")

  # conVert to data.table
    setDT(dicpes2008)
    setDT(dicdom2008)
    

# Indicate which columns will be read
    
    # indiViduals data
    mycolsPES <- c( "V0101", # year
                    "UF",    # State
                    "V0102", # Número de controle
                    "V0103", # Número de serie
                    "V0403", # Family number
                    "V4729", # Person Weight
                    "V4732", # Family Weight
                    "V8005", # age
                    "V0302", # Sex
                    "V0404", # race
                    "V4803", # Years of schooling
                    "V4745", # Educational attainment
                    "V4727", # Metropolitan area
                    "V9005", # Number of jobs
                    "V4805", # Employment Status
                    "V4706", # Job position
                    "V4810", # Job sectos
                    "V4721", # Household Income per Capita
                    "v4721", # Family Income per Capita
                    "V9054", # Work location
                    "V9055", # Work at home
                    "V9056", # Directo commute from home to work
                    "V9057", # Commute time
                    "V1409", # walking difficulties due to health condition
                    "V14091",# shoping difficulties due to health condition
                    "V1410", # ActiVe commute
                    "V1411", # ActiVe commute time
                    "V2801") # Smoker

    # household data
    mycolsDOM <- c("V0101", # year
                   "UF",    # State
                   "V0102", # Número de controle
                   "V0103", # Número de serie
                   "V0403", # Family number
                   "V4729", # Person Weight
                   "V4732", # Family Weight
                   "V4105", # Urban Vs Rural
                   "V2032", # Car or motorcycle ownership
                   "V4602", # Strata
                   "V4617", # STRAT
                   "V4609", # Population Projection
                   "UPA",   # Delimitação do município
                   "V4618", # PSU - Unidade primária de amostragem
                   "V4619", # Fator de subamostragem
                   "V4610", # InVerso da fração
                   "V4611", # Household weight
                   "V0233", # Household registered in 'saude da familia' program
                   "V0234") # time since registration
    
  # Filter selected Variables
    dicpes2008 <- dicpes2008[cod %in% mycolsPES, ]
    dicdom2008 <- dicdom2008[cod %in% mycolsDOM, ]
    
    
    
    
### Household data

    # create end position of Variables
      dicdom2008$fim <- dicdom2008$inicio + dicdom2008$tamanho -1
      head(dicdom2008)
      tail(dicdom2008)
      
    
    # Read the data file .txt file
      datafile <- "./PNAD2008/DOM2008.txt"
      
      pnad2008dom <- read_fwf(datafile,
                       fwf_positions(start=dicdom2008$inicio, 
                                     end=dicdom2008$fim, 
                                     col_names = dicdom2008$cod))

      
    # make sure all Variables are 'numeric' class
      pnad2008dom <- as.data.table(data.matrix(pnad2008dom))
    
# lower case column names 
  colnames(pnad2008dom) <- tolower(colnames(pnad2008dom))
      


### IndiViduals dataset

    # create end position of Variables
          dicpes2008$fim <- dicpes2008$inicio + dicpes2008$tamanho -1
          head(dicpes2008)
          tail(dicpes2008)
    

    # Read the data file .txt file
      datafile <- "./PNAD2008/PES2008.TXT"
      
      pnad2008pes <- read_fwf(datafile,
                 fwf_positions(start=dicpes2008$inicio, 
                               end=dicpes2008$fim, 
                               col_names = dicpes2008$cod))
      
    # make sure all Variables are 'numeric' class
      pnad2008pes <- as.data.table(data.matrix(pnad2008pes))

    # lower case column names 
      colnames(pnad2008pes) <- tolower(colnames(pnad2008pes))
      

# Join IndiViduals and Household data sets
  pnad2008 <- left_join(pnad2008pes, pnad2008dom)
  
# Clean memory
  rm(list=setdiff(ls(), c("pnad2008", "pnad2008dom")))
  gc(reset = T)
  




###### 3. Add VARiables to pnad2008 ---------------------

# year variable     
  pnad2008[, year := 2008]
  
# Count variable     
  pnad2008[, vcount := 1]
  table(pnad2008$vcount)

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

# Create  age groups with 5y intervals
  pnad2008$agegroup[pnad2008$v8005<5] <-"0-4"
  pnad2008$agegroup[pnad2008$v8005>4 & pnad2008$v8005<14] <-"5-13"
  pnad2008$agegroup[pnad2008$v8005>13 & pnad2008$v8005<18] <-"14-17"
  pnad2008$agegroup[pnad2008$v8005>17 & pnad2008$v8005<25] <-"18-24"
  pnad2008$agegroup[pnad2008$v8005>24 & pnad2008$v8005<30] <-"25-29"
  pnad2008$agegroup[pnad2008$v8005>29 & pnad2008$v8005<35] <-"30-34"
  pnad2008$agegroup[pnad2008$v8005>34 & pnad2008$v8005<40] <-"35-39"
  pnad2008$agegroup[pnad2008$v8005>39 & pnad2008$v8005<45] <-"40-44"
  pnad2008$agegroup[pnad2008$v8005>44 & pnad2008$v8005<50] <-"45-49"
  pnad2008$agegroup[pnad2008$v8005>49 & pnad2008$v8005<55] <-"50-54"
  pnad2008$agegroup[pnad2008$v8005>54 & pnad2008$v8005<60] <-"55-59"
  pnad2008$agegroup[pnad2008$v8005>59 & pnad2008$v8005<65] <-"60-64"
  pnad2008$agegroup[pnad2008$v8005>64 ] <-"65+"
  table(pnad2008$agegroup)


# Recode Urban vs Rural variable
  pnad2008[v4105<4, urban := "Urban"]
  pnad2008[v4105>3 & v4105<9, urban := "Rural"]
  table(pnad2008$urban)
  
  
# Recode Sex variable, make it compatible with PNAD
  pnad2008$v0302[pnad2008$v0302==2 ] <-"Men"
  pnad2008$v0302[pnad2008$v0302==4 ] <-"Women"
  table(pnad2008$v0302)

# Recode Education variable, make it compatible with PNAD
  pnad2008$v4745[pnad2008$v4745==1 ] <-"Uneducated"
  pnad2008$v4745[pnad2008$v4745==2 ] <-"Incomplete primary school"
  pnad2008$v4745[pnad2008$v4745==3 ] <-"Complete primary school"
  pnad2008$v4745[pnad2008$v4745==4 ] <-"Incomplete high school"
  pnad2008$v4745[pnad2008$v4745==5 ] <-"Complete high school"
  pnad2008$v4745[pnad2008$v4745==6 ] <-"Incomplete university degree"
  pnad2008$v4745[pnad2008$v4745==7 ] <-"University degree"
  pnad2008$v4745[pnad2008$v4745==8 ] <-NA
  table(pnad2008$v4745)



# Recode State variable, make it compatible with PNAD
  pnad2008[uf==11, UF :="Rondonia"]
  pnad2008[uf==12, UF :="Acre"]
  pnad2008[uf==13, UF :="Amazonas"]
  pnad2008[uf==14, UF :="Roraima"]
  pnad2008[uf==15, UF :="Para"]
  pnad2008[uf==16, UF :="Amapa"]
  pnad2008[uf==17, UF :="Tocantins"]
  pnad2008[uf==21, UF :="Maranhao"]
  pnad2008[uf==22, UF :="Piaui"]
  pnad2008[uf==23, UF :="Ceara"]
  pnad2008[uf==24, UF :="Rio Grande do Norte"]
  pnad2008[uf==25, UF :="Paraiba"]
  pnad2008[uf==26, UF :="Pernambuco"]
  pnad2008[uf==27, UF :="Alagoas"]
  pnad2008[uf==28, UF :="Sergipe"]
  pnad2008[uf==29, UF :="Bahia"]
  pnad2008[uf==31, UF :="Minas Gerais"]
  pnad2008[uf==32, UF :="Espirito Santo"]
  pnad2008[uf==33, UF :="Rio de Janeiro"]
  pnad2008[uf==35, UF :="Sao Paulo"]
  pnad2008[uf==41, UF :="Parana"]
  pnad2008[uf==42, UF :="Santa Catarina"]
  pnad2008[uf==43, UF :="Rio Grande do Sul"]
  pnad2008[uf==50, UF :="Mato Grosso do Sul"]
  pnad2008[uf==51, UF :="Mato Grosso"]
  pnad2008[uf==52, UF :="Goias"]
  pnad2008[uf==53, UF :="Federal District"]
  table(pnad2008$UF)



# Create variable Metropolitan area
  pnad2008$metro[pnad2008$v4727 !=1 ] <-"Non-metropolitan area"
  pnad2008$metro[pnad2008$uf==15 & pnad2008$v4727==1] <-"Belem"
  pnad2008$metro[pnad2008$uf==23 & pnad2008$v4727==1] <-"Fortaleza"
  pnad2008$metro[pnad2008$uf==26 & pnad2008$v4727==1] <-"Recife"
  pnad2008$metro[pnad2008$uf==29 & pnad2008$v4727==1] <-"Salvador"
  pnad2008$metro[pnad2008$uf==31 & pnad2008$v4727==1] <-"Belo Horizonte"
  pnad2008$metro[pnad2008$uf==33 & pnad2008$v4727==1] <-"Rio de Janeiro"
  pnad2008$metro[pnad2008$uf==35 & pnad2008$v4727==1] <-"Sao Paulo"
  pnad2008$metro[pnad2008$uf==41 & pnad2008$v4727==1] <-"Curitiba"
  pnad2008$metro[pnad2008$uf==43 & pnad2008$v4727==1] <-"Porto Alegre"
  pnad2008$metro[pnad2008$uf==53 & pnad2008$v4727==1] <-"Federal District"
  table(pnad2008$metro)

gc(reset = T)



# Delete Missing values from Income variable (v4721 Monthly household income per capita)
# to create new variable of income deciles
#Antes da limpeza==391.868 obs. A lipeza tirou 11.573 cases -> 380.295

summary(pnad2008$v4721)
pnad2008 <- pnad2008[pnad2008$v4721<999999999999, ] #elimina observacoes missing na var.  de renda Dom per capita.
pnad2008 <- subset(pnad2008, v4721>= 0) #elimina observacoes NA
summary(pnad2008$v4721)

# Create  var. income deciles of Monthly household income per capitade
  pnad2008[, decileBR:= as.numeric( cut(v4721, breaks=quantile(v4721,
                                                               probs=seq(0, 1, by=0.1), na.rm=T),
                                        include.lowest= TRUE, labels=1:10))]
  
  # Checking Table
  table(pnad2008$decileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo

  
# Create  var. income quintile of Monthly household income per capitade
 pnad2008[, quintileBR:= as.numeric( cut(v4721, breaks=quantile(v4721,
                                                                probs=seq(0, 1, by=0.2), na.rm=T),
                                         include.lowest= TRUE, labels=1:5))]
  
  
 # function to Create Quintile for different regions
 funQuintReg <- function(x){quintileRegion <- as.numeric(cut(x[, v4721], labels=c(1:5),
                                                             breaks=quantile(x[, v4721], seq(0,1, 0.2), na.rm=T),
                                                             include.lowest = TRUE))
                                                       cbind(x, quintileRegion)
                                                        }
 
 # function to Create Quartile for different regions
 funQuartReg <- function(x){quartileRegion <- as.numeric(cut(x[, v4721], labels=c(1:4),
                                                             breaks=quantile(x[, v4721], seq(0,1, 0.25), na.rm=T),
                                                             include.lowest = TRUE))
                                                         cbind(x, quartileRegion)
                                                         }
 
 
 # function to Create Quintile for different Metro Areas
 funQuintMetro <- function(x){quintileMetro <- as.numeric(cut(x[, v4721], labels=c(1:5),
                                                             breaks=quantile(x[, v4721], seq(0,1, 0.2), na.rm=T),
                                                             include.lowest = TRUE))
 cbind(x, quintileMetro)
 }
 
 # function to Create Quartile for different Metro Areas
 funQuartMetro <- function(x){quartileMetro <- as.numeric(cut(x[, v4721], labels=c(1:4),
                                                             breaks=quantile(x[, v4721], seq(0,1, 0.25), na.rm=T),
                                                             include.lowest = TRUE))
 cbind(x, quartileMetro)
 }
 
 
 # create regional income deciles, quintiles and quartiles
 pnad2008 <- do.call(rbind, lapply(split(pnad2008, pnad2008[, region]), funQuintReg)) ; gc(reset = T)
 pnad2008 <- do.call(rbind, lapply(split(pnad2008, pnad2008[, region]), funQuartReg)) ; gc(reset = T)
 pnad2008 <- do.call(rbind, lapply(split(pnad2008, pnad2008[, metro]), funQuintMetro)) ; gc(reset = T)
 pnad2008 <- do.call(rbind, lapply(split(pnad2008, pnad2008[, metro]), funQuartMetro)) ; gc(reset = T)
 
 head(pnad2008)
 
 # 2-Way Frequency Table -  Numero de casos dentro de cada Decil tem que ser igual/proximo
 table(pnad2008$quintileRegion, pnad2008$region)
 table(pnad2008$quartileRegion, pnad2008$region)
 
 table(pnad2008$quintileMetro, pnad2008$metro)
 table(pnad2008$quartileMetro, pnad2008$metro)
 
 table(pnad2008$quartileMetro, pnad2008$dummyVehicle, pnad2008$metro)
 
 
 
# Create modified commute and Active Travel variables 
# # Para pessoas ocupadas e q nao responderam a v1410, imputa q desloc ativo=0
# # Porque?: Se faz isso apenas para elas contarem no denominador na hora de calcular a proporcao de desloca ativo
# # inclui na var v1410 1.393 casos 

# Impute commute time '0' no active commute IF person works from home
  pnad2008[v9057 == 0, v9057 :=NA]
  pnad2008[, v9057mod := ifelse(v9054==3 , NA, v9057)]
  table(pnad2008$v9057mod)
  table(pnad2008$v9057)

# Impute no active commute IF person works from home, or has a job but did not answered the question of active tralvel (probably due to health issues)
  pnad2008[v1410 == 0, v1410 :=NA]
  pnad2008[, v1410mod := ifelse(v9054==3, 4, v1410)]
  table(pnad2008$v1410)
  table(pnad2008$v1410mod)
  

#  Impute active commute time '0' IF person works from home
  pnad2008[v1411 == 0, v1411 :=NA]
  pnad2008[, v1411mod := ifelse(v9054==3, NA, v1411)]
  table(pnad2008$v1411)
  table(pnad2008$v1411mod)


# create indicator variable of ind. that practice active travel for > 30minutes when commuting to work
  pnad2008[, actv_commutetime30 := ifelse(v1410==2 & v1411 >3, 1, 0)]
  table(pnad2008$actv_commutetime30)


# now create the pre-stratified weight to be used in all of the survey designs
  pnad2008[, pre_wgt  := v4619 * v4610]
  summary(pnad2008$pre_wgt)


#Recode Active Travel variable P040 into string
  pnad2008$v1410[pnad2008$v1410==2] <- "Yes"
  pnad2008$v1410[pnad2008$v1410==4] <- "No"
  table(pnad2008$v1410)

# vehicle ownership variable, make it compatible with PNAD
  pnad2008$v2032[pnad2008$v2032==2 ] <-"Car"
  pnad2008$v2032[pnad2008$v2032==4 ] <-"Motorcycle"
  pnad2008$v2032[pnad2008$v2032==6 ] <-"Car + Motorcycle"
  pnad2008$v2032[pnad2008$v2032==8 ] <-"None"
  table(pnad2008$v2032)

  # Dummy for Vehicle ownership Variable, make it compatible with PNAD
  pnad2008[, dummyVehicle := ifelse(v2032=="None" , 0, 1)] # If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit
  
  pnad2008$dummyVehicle <- factor(pnad2008$dummyVehicle, levels=c(1,0),
                                    labels=c("Yes","No"))
  
  
  table(pnad2008$dummyVehicle)
  
  
# limpa memoria
  gc(reset = T) 
  
  

# 
# # Cria var tempo de desloc em min.
# assign(pnad2008 , get(pnad2008) %>% mutate(commutetime = ifelse(v9057== 0,0,
#                                                   ifelse(v9057== 1,15,
#                                                   ifelse(v9057== 3,45,
#                                                   ifelse(v9057== 5,90,
#                                                   ifelse(v9057==7,120,NA))))))

  ########## REcode Housegold DAta  ----------------
  
  pnad2008dom[uf < 20, region :="North"]
  pnad2008dom[uf > 20 & uf < 30, region :="Northeast"]
  pnad2008dom[uf > 30 & uf < 40, region :="Southeast"]
  pnad2008dom[uf > 40 & uf < 50, region :="South"]
  pnad2008dom[uf > 50 & uf < 60, region :="Midwest"]
  table(pnad2008dom$region)
  
  # urban x rural
  pnad2008dom$urban[pnad2008dom$v4105<4] <-"Urban"
  pnad2008dom$urban[pnad2008dom$v4105>3 & pnad2008dom$v4105<9] <-"Rural"
  table(pnad2008dom$urban)
  
  # vehicle ownership variable, make it compatible with PNAD
  pnad2008dom$v2032[pnad2008dom$v2032==0 ] <-NA
  pnad2008dom$v2032[pnad2008dom$v2032==2 ] <-"Car"
  pnad2008dom$v2032[pnad2008dom$v2032==4 ] <-"Motorcycle"
  pnad2008dom$v2032[pnad2008dom$v2032==6 ] <-"Car + Motorcycle"
  pnad2008dom$v2032[pnad2008dom$v2032==8 ] <-"None"
  table(pnad2008dom$v2032)
  
  # Dummy for Vehicle ownership Variable, make it compatible with PNAD
  pnad2008dom[, dummyVehicle := ifelse(v2032=="None" , 0, 1)] # If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit
  
  pnad2008dom$dummyVehicle <- factor(pnad2008dom$dummyVehicle, levels=c(1,0),
                                  labels=c("Yes","No"))
  
  table(pnad2008dom$dummyVehicle)
  
  
###### 4. Save Pnad2008 data set as CSv file ------------

  # fwrite(pnad2008, file.path="./pnad2008.csv")
  # fwrite(pnad2008dom, file.path="./pnad2008dom.csv")
gc(reset = T)


saveRDS(pnad2008, file="pnad2008.Rds")
saveRDS(pnad2008dom, file="pnad2008dom.Rds")



############## 5. PNAD Create survey design  -----------

#load data set
  pnad2008 <- fread("pnad2008.csv")
  pnad2008dom <- fread("pnad2008dom.csv")
 
  # system.time(pnad2008dam <- fread("pnad2008dam.csv"))
  # pnad2008 <- select(pnad2008, v0101,uf,v0102,v0103,v0403,v4729,v4609, v4732,v8005,v0302,v0404,v4803,v4838,v4728,v4727,v0602,v9005,v4805,v4706,v4810,v4720,v4721,v4721,v9054,v9055,v9056,v9057,v1409,v14091,v1410,v1411, v4602, v4618, v4617, upa, region, AGE,agegroup,DecileBR,vcount, v1410mod,v1411mod,v9057mod,actv_commutetime30)

#Create Sample Design object of pnad2008

  #define como imputar variancia quando houver apenas um domicilio (PSU) no estrato
  options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu 
  
  
  #Cria objeto de desenho da amostra                      
  sample.pnad08 <- svydesign(data = pnad2008,
                             id = ~v4618, #PSU
                             strata = ~v4617, #Strat
                             weights = ~pre_wgt , #person weight # ? pre_wgt ?v4729
                             nest = TRUE
                             )
  
  # postStratify pnad2008 (Djalma suggestion)
  post.pop <- unique(data.frame(v4609=as.character(pnad2008$v4609), Freq= as.numeric(pnad2008$v4609)))
  sample.pos <- postStratify(design=sample.pnad08, strata=~v4609, population=post.pop)
  
  #Subset Sample of survey design for aged above 14 years old
  sample.pnad08.14y <- subset(sample.pos, v8005>=14)
  
  
  
# survey design of Household Dataset ----
  
  # This eliminates observations with missing values in the weight variable
    pnad2008dom <- pnad2008dom[!(is.na(pnad2008dom$v4611))]
  
  options( survey.lonely.psu = "adjust" )
  sample.pnad08dom <- svydesign(data = pnad2008dom,
                                id = ~v4618, #PSU
                                strata = ~v4617, #Strat
                                weights = ~v4611, #household weight
                                nest = TRUE)
  
  
###################### Check PNAD08 Results IBGE report ###################### 
# Small differences from table, probably due to New Sample Weights updated after the 2010 Pop Census


# Check against Table 4.2 (IBGE report), available at http://www.ibge.gov.br/home/estatistica/populacao/panorama_saude_brasil_2003_2008/tab4_2.pdf
# Check against Table 4.3 (IBGE report), available at http://www.ibge.gov.br/home/estatistica/populacao/panorama_saude_brasil_2003_2008/tab4_3.pdf
# Pop that practice Active travel (v1410==2)

      # Absolut number | Brasil - 30,565 
      svytable(~v1410=="Yes", design=sample.pnad08.14y)# ok 30,884 
        
      # % Brazil - 33.4%
      svyciprop(~v1410=="Yes", design= sample.pnad08.14y) # ok 33.8%
      
      
      # Absolut number per Region | North - 2.522 , South - 5,094
      svytable(~factor(v1410=="Yes")+region, design=sample.pnad08.14y)
      
      # % per region | North - 37.3 , South - 35.0%
      svyby(~ v1410=="Yes", ~region, design= sample.pnad08.14y,  vartype="ci",  level = 0.95,  svyciprop)



      # Absolut number per Sex
      svytable(~factor(v1410=="Yes")+v0302, design=sample.pnad08.14y)
      
      # % per sex
      svyby(~factor(v1410=="Yes"), ~v0302, design= sample.pnad08.14y,  vartype="ci",  level = 0.95,  svyciprop)
      


# check Household results ----------
# http://biblioteca.ibge.gov.br/visualizacao/livros/liv44356.pdf
      
    # Tabela 1.4 - Absolut number per Region 
    # South: 4,525 , North: 2,047 , Midwest: 2,086
    svytable(~factor(v0233==1)+region, design=sample.pnad08dom)
    
    
    
    # Gráfico 3 - % per region
      # South: 50.35 , North: 51.0 , Midwest: 49.1
      
      svyby(~factor(v0233==1),  ~region, design= sample.pnad08dom,  vartype="ci",  level = 0.95,svyciprop)
    

      
###################### Check Pnad2008 Results Knuth (2011) ###################### 


# paper of Knuth et al (2011), available at http://www.scielo.br/scielo.php?script=sci_arttext&pid=S1413-81232011001000007

# todos valores absolutos e propocoes ficam muito proximos, mas nunca batem perfeitamente
# Acredito que seja por conta da reponderacao que o IBGE fez apÃ³s o Censo 2010


      sample.pnad08.14y

#Table 1
#total pop > 14 y.old 142,533 x 143,200
svytable(~v0101, design=sample.pnad08.14y) #regiao [Meu total==139,917,340, Knuth Total==142.533.480]

#Table 1 - ... by region
svytable(~v0101+region, design=sample.pnad08.14y)

#Table 1 - ... by sex
svytable(~v0101+v0302, design=sample.pnad08.14y)


#Tabela 2 da Knuth et al (2011), pratica de transporte ativo >30min both ways


#Region
svytable(~factor(v1411>3)+region, design=sample.pnad08.14y)

svyby(~factor( v1411>3 ) ,
      ~region ,  
      design = sample.pnad08.14y,  
      vartype="ci",  level = 0.95,  svyciprop)






#Sexo
svytable(~factor(v1411>3)+v0302, design=sample.pnad08.14y)

svyby(~factor( v1411>3 ) ,
      ~v0302 ,  
      design = sample.pnad08.14y,  
      vartype="ci",  level = 0.95,  svyciprop
)






#Age
svyby(
  ~factor( v1411>3 ) ,
  ~AGE ,  
  design = sample.pnad08.14y,  
  vartype="ci",  level = 0.95,  svyciprop
)

