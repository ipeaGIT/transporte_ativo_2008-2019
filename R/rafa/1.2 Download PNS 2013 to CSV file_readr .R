
# Damico
https://github.com/ajdamico/asdfree/tree/master/Pesquisa%20Nacional%20De%20Saude


# This script Downloads data from PNS 2013 to analyze Active Travel in Brazil
  
  # 1: Donwload, read Pns2013 DATA and save it as .csv file 
  # 2: Merge household and individual data sets
  # 3: Add Variables to pns2013 data
  # 4: Create survey design for PNS 2013 data

# Oxford 07/10/2015
# R version:  RRO 3.2.2 (64 bits)
# by Rafael Pereira - urbandemographics.blogspot.com





# Set working directory ------------
setwd("C:/Users/rafa/Desktop/ActiveTravel_Pnad-PNS")


###### Load Packages -----------






# 1:Donwload, save and read Pns2013 DATA----------------
    
# Download and unzip file
  download.file("ftp://ftp.ibge.gov.br/PNS/2013/microdados/pns_2013_microdados_2016_06_30.zip", "PNS2013.zip", quiet = FALSE)
  dir.create("PNS2013")
  unzip("PNS2013.zip", exdir="PNS2013", junkpaths=T)

#Household data - Read .txt data using SAS instructions
    sas_file <- "./PNS2013/input DOMPNS2013.sas" #SAS instructions
    sas_file <- parse.SAScii(sas_file)
    datafile <- "./PNS2013/DOMPNS2013.txt"
    
    # Read the .txt file
      pns2013dom <-   read_fwf(datafile,
                               fwf_widths(dput(sas_file$width),
                                          col_names=(dput(sas_file$varname))),
                               progress = interactive())
    
    # make sure all variables are 'numeric' class
      pns2013dom <- as.data.table(data.matrix(pns2013dom))
      

    # clean memory
      gc(reset = T)

      
#Individuals data - Read .txt data using SAS instructions
    sas_file <- "./PNS2013/input PESPNS2013.sas" #SAS instructions
    sas_file <- parse.SAScii(sas_file)
    datafile <- "./PNS2013/PESPNS2013.txt"
    
    # Read the .txt file
    pns2013pes <-   read_fwf(datafile,
                       fwf_widths(dput(sas_file$width),
                                  col_names=(dput(sas_file$varname))),
                       progress = interactive())
    
    # make sure all variables are 'numeric' class
    pns2013pes <- as.data.table(data.matrix(pns2013pes))
    
# clean memory
  rm(list=setdiff(ls(), c("pns2013pes", "pns2013dom")))
  gc(reset = T)

  

########## 2. Recode Household data  ----------------

  # year variable     
    pns2013dom[, year := 2013]
  
  
  # Urban vs Rural areas
    pns2013dom$urban[pns2013dom$V0026==1 ] <-"Urban"
    pns2013dom$urban[pns2013dom$V0026==2 ] <-"Rural"
  
  # Vehicle ownership Variable, make it compatible with PNAD
    pns2013dom$v2032[pns2013dom$A01817 ==2 & pns2013dom$A020 >0 ] <- "Car" #  2
    pns2013dom$v2032[pns2013dom$A01817 ==1 & pns2013dom$A020 <1 ] <- "Motorcycle" #  4 
    pns2013dom$v2032[pns2013dom$A01817 ==1 & pns2013dom$A020 >0 ] <- "Car + Motorcycle" # 6 
    pns2013dom$v2032[pns2013dom$A01817 ==2 & pns2013dom$A020 <1 ] <- "None" # 8


  # Dummy for Vehicle ownership Variable, make it compatible with PNAD
    pns2013dom[, dummyVehicle := ifelse(v2032=="None" , 0, 1)] # If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit
    
    
    pns2013dom$dummyVehicle <- factor(pns2013dom$dummyVehicle, levels=c(1,0),
                               labels=c("Yes","No"))
    
    table(pns2013dom$dummyVehicle)
    

    
# 3. Merge household and individual data sets---------

# Merge datasets
  pns2013a <- left_join(pns2013pes, pns2013dom)

# clean memory
  rm(list=setdiff(ls(), c("pns2013", "pns2013dom")))
  gc(reset = T)


# 4: Add Variables to pns2013 data ---------------------------------

  # year variable     
  pns2013[, year := 2013]
  
# Count variable     
  pns2013[, vcount := 1]
  table(pns2013$vcount)

  # cria VariaVel de Regiao
    pns2013[V0001 < 20, region :="North"]
    pns2013[V0001 > 19 & V0001 < 30, region :="Northeast"]
    pns2013[V0001 > 29 & V0001 < 40, region :="Southwest"]
    pns2013[V0001 > 39 & V0001 < 50, region :="South"]
    pns2013[V0001 > 49 & V0001 < 60, region :="Midwest"]
    table(pns2013$region)    
    
    

  # Create age groups with bigger age interval
    pns2013[C008>=0 & C008<18, AGE :="0-17"]
    pns2013[C008>17 & C008<25, AGE :="18-24"]
    pns2013[C008>24 & C008<35, AGE :="25-34"]
    pns2013[C008>34 & C008<45, AGE :="35-44"]
    pns2013[C008>44 & C008<55, AGE :="45-54"]
    pns2013[C008>54 & C008<65, AGE :="55-64"]
    pns2013[C008>64,  AGE :="65+"]
    table(pns2013$AGE)
    
  # Create  age groups with 5y intervals
    pns2013[C008 <5, agegroup := "0-4"]
    pns2013[C008 >4 & C008 <14, agegroup := "5-13"]
    pns2013[C008 >13 & C008 <18, agegroup := "14-17"]
    pns2013[C008 >17 & C008 <25, agegroup := "18-24"]
    pns2013[C008 >24 & C008 <30, agegroup := "25-29"]
    pns2013[C008 >29 & C008 <35, agegroup := "30-34"]
    pns2013[C008 >34 & C008 <40, agegroup := "35-39"]
    pns2013[C008 >39 & C008 <45, agegroup := "40-44"]
    pns2013[C008 >44 & C008 <50, agegroup := "45-49"]
    pns2013[C008 >49 & C008 <55, agegroup := "50-54"]
    pns2013[C008 >54 & C008 <60, agegroup := "55-59"]
    pns2013[C008 >59 & C008 <65, agegroup := "60-64"]
    pns2013[C008 >64, agegroup := "65+"]
    table(pns2013$agegroup)

    
# Create BMI  - Body Max Index (weight / height)
    summary(pns2013$P00101) # weight
    summary(pns2013$P00401) # height
    pns2013[, P00401 := ifelse(P00401==0 , NA, P00401/100)] # If person declared height of 0 cm, consider it a missing value, otherwise, convert it to meters unit
    
    #compute BMI    
    pns2013[, bmi := P00101 / P00401^2 ]
    summary(pns2013$bmi)
    plot(pns2013$bmi)
    

    
    
    
    
    
    # pns2013$age_cat <- factor( 1 + findInterval( as.numeric( pns2013$C008 ) , c( 5 , 14, 18, 25, 30, 35,40,45,50,55,60) ) , 
    #                            labels = c( "0-4", "5-13", "14-17", "18-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54","55-59", "60+"))
    # table(pns2013$age_cat)    
    # 
    

  # Recode Education Variable, make it compatible with PNAD
    pns2013$v4745[pns2013$VDD004==1 ] <-"Uneducated"
    pns2013$v4745[pns2013$VDD004==2 ] <-"Incomplete primary school"
    pns2013$v4745[pns2013$VDD004==3 ] <-"Complete primary school"
    pns2013$v4745[pns2013$VDD004==4 ] <-"Incomplete high school"
    pns2013$v4745[pns2013$VDD004==5 ] <-"Complete high school"
    pns2013$v4745[pns2013$VDD004==6 ] <-"Incomplete university degree"
    pns2013$v4745[pns2013$VDD004==7 ] <-"University degree"
    table(pns2013$v4745)
    is.factor(pns2013$v4745)
    levels(pns2013$v4745)

    levels(pns2013$v4745) <- reorder(x=c("Uneducated", "Incomplete primary school", "Complete primary school", "Incomplete high school", "Complete high school", "Incomplete university degree", "University degree"),
                                     X=sort(unique(pns2013$VDD004)))
    

    
    table(pns2013$v4745)
    
 # Educational groups
    pns2013[ VDD004 <3, edugroup := "Uneducated + Incomplete primary school"]
    pns2013[ VDD004 ==3 | VDD004 ==4, edugroup := "Complete primary school"]
    pns2013[ VDD004 ==5 | VDD004 ==6, edugroup := "Complete high school"]
    pns2013[ VDD004 ==7, edugroup := "University degree"]

    pns2013$edugroup <- factor(pns2013$edugroup, levels=c("Uneducated + Incomplete primary school", "Complete primary school", "Complete high school", "University degree"),
                   labels=c("Uneducated + Incomplete primary school", "Complete primary school", "Complete high school", "University degree"))
    
table(pns2013$edugroup)

    # Recode Race variable into string
    pns2013$C009[pns2013$C009==1] <-"White"
    pns2013$C009[pns2013$C009==2] <-"Black"
    pns2013$C009[pns2013$C009==3] <-"Asian"
    pns2013$C009[pns2013$C009==4] <-"Brown"
    pns2013$C009[pns2013$C009==5] <-"Indigenous"
    pns2013$C009[pns2013$C009==9] <- NA
    table(pns2013$C009)


  # Recode Sex Variable, make it compatible with PNAD
    pns2013$v0302[pns2013$C006==1 ] <-"Men"
    pns2013$v0302[pns2013$C006==2 ] <-"Women"
    table(pns2013$v0302)

  # Recode Urban Rural Variable, make it compatible with PNAD
    pns2013$urban[pns2013$V0026==1 ] <-"Urban"
    pns2013$urban[pns2013$V0026==2 ] <-"Rural"
        
    
  # Recode Metropolitan area Variable, make it compatible with PNAD
    pns2013$v4727[pns2013$V0031==1 | pns2013$V0031==2] <- 1
    pns2013$v4727[pns2013$V0031>2 ] <- 3
  

  # Create Variable Metropolitan area
    pns2013$metro[pns2013$V0031==4] <- "Non-metropolitan area"
    pns2013$metro[pns2013$V0001==15 & pns2013$V0031<3] <-"Belem"
    pns2013$metro[pns2013$V0001==23 & pns2013$V0031<3] <-"Fortaleza"
    pns2013$metro[pns2013$V0001==26 & pns2013$V0031<3] <-"Recife"
    pns2013$metro[pns2013$V0001==29 & pns2013$V0031<3] <-"Salvador"
    pns2013$metro[pns2013$V0001==31 & pns2013$V0031<3] <-"Belo Horizonte"
    pns2013$metro[pns2013$V0001==33 & pns2013$V0031<3] <-"Rio de Janeiro"
    pns2013$metro[pns2013$V0001==35 & pns2013$V0031<3] <-"Sao Paulo"
    pns2013$metro[pns2013$V0001==41 & pns2013$V0031<3] <-"Curitiba"
    pns2013$metro[pns2013$V0001==43 & pns2013$V0031<3] <-"Porto Alegre"
    pns2013$metro[pns2013$V0001==53 & pns2013$V0031<3] <-"Federal District"
    
    
    

  # Recode Active Travel Variable, make it compatible with PNAD
    pns2013$v1410[pns2013$P040==1 | pns2013$P040==2] <- "Yes"
    pns2013$v1410[pns2013$P040==3 ] <- "No"
    table(pns2013$v1410)


  ### create indicator variable of ind. above 18yearsold that practice active travel for > 30minutes
  ## this is the definition used in table 3.4.1.1 of IBGE report
    pns2013 <- transform(pns2013, actv_commutetime =       ifelse( is.na(P04101),0, P04101 * 60 + P04102)) # Active commute time
    pns2013 <- transform(pns2013, actv_traveltimehabacts = ifelse( is.na(P04301),0, P04301 * 60 + P04302)) #active travel time to habitual activities, such as going to or taking someone to school 
    pns2013 <- transform(pns2013, total_actvtraveltime = actv_commutetime + actv_traveltimehabacts) ## total active travel time
    pns2013 <- transform(pns2013, physicallyactive30 = ifelse(total_actvtraveltime >= 30,1,0)) # total active travel time >30 (1,0)
    pns2013 <- transform(pns2013, actv_commutetime30 = ifelse(actv_commutetime >= 30,1,0)) #commute time >30 (1,0)
  
    table(pns2013$P040)
    table(pns2013$actv_commutetime30)
  
  
  # Recode Acctive Travel Variable P040 into string
    pns2013$P040[pns2013$P040==1] <- "Yes, all the journey"
    pns2013$P040[pns2013$P040==2] <- "Yes, part of the journey"
    pns2013$P040[pns2013$P040==3 ] <- "No"
    table(pns2013$P040)


    
    

    
=====================================

  # Delete Missing values from Income variable (v4721 Monthly household income per capita)
  # to create new variable of income deciles
  #Antes da limpeza==391.868 obs. A limpeza tirou 11.573 cases -> 380.295
    
    
### Income Variables
      
  # Summary of income variables
    summary(pns2013$E01602)
    summary(pns2013$E01604)
    summary(pns2013$E01802)
    summary(pns2013$E01804)
    
      # # function to sum row values ignoring NAs
      #   # http://stackoverflow.com/questions/13106645/using-in-data-table-to-sum-the-values-of-two-columns-in-r-ignoring-nas
      # `%+na%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, x+y) )}
      # 
      # pns2013[, totalincome2 := E01602 %+na% E01604 %+na% E01802 %+na% E01804 ]
      # summary(pns2013$totalincome2)

    # # identify all income sources
    #   colsToSum <- c("E01602" , "E01604" , "E01802" , "E01804")
    # 
    # # Create column with total income of each individual
    #   pns2013[, totalincome := rowSums(pns2013[, colsToSum, with=FALSE], na.rm = T)] # sum all income sources
    #   summary(pns2013$totalincome)


    # Create column with total household income
      pns2013[, totalhouseholdincome := sum(E01602, E01604 , E01802, E01804, na.rm = T) , # sum income from all sources
              by=.(V0001, V0024, UPA_PNS, V0006_PNS, UPA, V00281, V00283, VDC001)]      # by household
    
    summary(pns2013$totalhouseholdincome)
    head(table(pns2013$totalhouseholdincome))
    
    
    # Create Household Income per Capita, compatible with PNAD 2008 data
     pns2013[, v4721 := totalhouseholdincome / C001 ] # C001  VDC001
     summary(pns2013$v4721)
      
    
    pns2013[totalhouseholdincome > 100000, .(V0022,VDC001,  totalhouseholdincome, v4721, C001)]
    
    
    
    
    ## \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    pns2013 <- pns2013[v4721 > 0,] 

      

      
      
======================================
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
    pns2013 <- do.call(rbind, lapply(split(pns2013, pns2013[, region]), funQuintReg)) ; gc(reset = T)
    pns2013 <- do.call(rbind, lapply(split(pns2013, pns2013[, region]), funQuartReg)) ; gc(reset = T)
    pns2013 <- do.call(rbind, lapply(split(pns2013, pns2013[, metro]), funQuintMetro)) ; gc(reset = T)
    pns2013 <- do.call(rbind, lapply(split(pns2013, pns2013[, metro]), funQuartMetro)) ; gc(reset = T)
    
    head(pns2013)
    
    table(pns2013$quintileRegion) #Numero de casos dentro de cada Decil tem que ser igual/proximo
    
    # number of cases in each Metro area by income quartile
    table(pns2013$quartileRegion, pns2013$metro) #Numero de casos dentro de cada Decil tem que ser igual/proximo
    
    
    
    
    
    
    
    
    
    
    
    
    
  
    
    
########## 5. Save modified DAta files  ----------------

    saveRDS(pns2013, file="pns2013.Rds")
    saveRDS(pns2013dom, file="pns2013dom.Rds")
    
    # pns2013 <- readRDS("pns2013.Rds")
    
fwrite(pns2013, file.path="pns2013.csv")
fwrite(pns2013dom, file.path="pns2013dom.csv")
gc(reset=T) # clean memory

beep()
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


