# Load libraries -----

# https://www.ibge.gov.br/estatisticas/sociais/educacao/9127-pesquisa-nacional-por-amostra-de-domicilios.html?edicao=18338&t=microdados
rm(list=ls())
gc(reset=TRUE)
library(PNADcIBGE) # devtools::install_github("Gabriel-Assuncao/PNADcIBGE")
library(survey)
library(srvyr)
library(data.table)
library(microdadosBrasil) # devtools::install_github("lucasmation/microdadosBrasil")

############## PNAD Create survey design  -----------

#load data set
pnad2008 <- readr::read_rds("../../data/transporte_ativo_2008-2019/pnad2008.rds")
pnad2008dom <- readr::read_rds("../../data/transporte_ativo_2008-2019/pnad2008dom.rds")

# system.time(pnad2008dam <- fread("pnad2008dam.csv"))
# pnad2008 <- select(pnad2008, v0101,uf,v0102,v0103,v0403,v4729,v4609, v4732,v8005,v0302,v0404,v4803,v4838,v4728,v4727,v0602,v9005,v4805,v4706,v4810,v4720,v4721,v4721,v9054,v9055,v9056,v9057,v1409,v14091,v1410,v1411, v4602, v4618, v4617, upa, region, AGE,agegroup,DecileBR,vcount, v1410mod,v1411mod,v9057mod,actv_commutetime30)

# Create Sample Design object of pnad2008

# define como imputar variancia quando houver apenas um domicilio (PSU) no estrato
srvyr::opti
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu 


# Cria objeto de desenho da amostra                      
sample.pnad08 <- survey::svydesign(data = pnad2008,
                           id = ~v4618, #PSU
                           strata = ~v4617, #Strat
                           weights = ~pre_wgt , #person weight # ? pre_wgt ?v4729
                           nest = TRUE)

# postStratify pnad2008 (Djalma suggestion)
# v4609 => projecao da populacao
post.pop <- data.frame(v4609 = as.character(pnad2008$v4609)
                       , Freq = as.numeric(pnad2008$v4609))
post.pop <- unique(post.pop)
sample.pos <- survey::postStratify(design=sample.pnad08
                                   , strata=~v4609
                                   , population=post.pop)

# Subset Sample of survey design for aged above 14 years old
sample.pnad08.14y <- subset(sample.pos, v8005>=14)



# survey design of Household Dataset ----

# This eliminates observations with missing values in the weight variable
pnad2008dom <- pnad2008dom[!is.na(v4611),]

options( survey.lonely.psu = "adjust" )
sample.pnad08dom <- survey::svydesign(data = pnad2008dom,
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
survey::svytable(~v1410 == "Yes", design = sample.pnad08.14y) # rafa 30,884 joao 30,760
              

# % Brazil - 33.4%
survey::svyciprop(~v1410 =="Yes", design = sample.pnad08.14y) # rafa 33.8% joao 33.8%


# Absolut number per Region 
# rafa | North - 2.522 , South - 5,094
# joao | North - 2.677 , South - 5,076
survey::svytable(~factor(v1410 == "Yes") + region, design = sample.pnad08.14y)

# % per region 
# rafa | North - 37.3 , South - 35.0%
# joao | North - 38.6 , South - 35.4%
survey::svyby(~ v1410 == "Yes", ~region, design = sample.pnad08.14y
              ,  vartype = "ci", level = 0.95,  svyciprop)



# Absolut number per Sex
# joao | Men 17936737 Women 12823519
survey::svytable(~factor(v1410 == "Yes") + v0302, design = sample.pnad08.14y)

# % per sex
# joao | Women 0.3334015  Men 0.3413390 
survey::svyby(~factor(v1410 == "Yes"), ~v0302,
              design = sample.pnad08.14y,  vartype = "ci",  level = 0.95,  svyciprop)



# check Household results ----------
# http://biblioteca.ibge.gov.br/visualizacao/livros/liv44356.pdf

# Tabela 1.4 - Absolut number per Region 
# rafa | South: 4,525 , North: 2,047 , Midwest: 2,086
# joao | South: 4,499 , North: 2,101 , Midwest: 1,563
#          factor(v0233 == 1)  Midwest    North Northeast    South Southeast
#          FALSE               2019134  2051519   5193236  4430409  16371213
#          TRUE                1563462  2101351   9636517  4498891   9083157
survey::svytable(~factor(v0233 == 1) + region, design = sample.pnad08dom)



# Grafico 3 - % per region
# Rafa | South: 50.35 , North: 51.0 , Midwest: 49.1
# Joao | South: 50,38 , North: 50,6 , Midwest: 43,6

survey::svyby(~factor(v0233 == 1),  ~region, design = sample.pnad08dom
              ,  vartype="ci",  level = 0.95, svyciprop)



###################### Check Pnad2008 Results Knuth (2011) ###################### 


# paper of Knuth et al (2011), available at http://www.scielo.br/scielo.php?script=sci_arttext&pid=S1413-81232011001000007

# todos valores absolutos e propocoes ficam muito proximos, mas nunca batem perfeitamente
# Acredito que seja por conta da reponderacao que o IBGE fez após o Censo 2010


sample.pnad08.14y

#Table 1
# rafa | total pop > 14 y.old 142,533 x 143,200 
# rafa | regiao [Meu total == 139,917,340, Knuth Total == 142.533.480]
# Joao | regiao [Meu total == 146,212,852, Knuth Total == 142.533.480]
survey::svytable(~v0101, design=sample.pnad08.14y)

#Table 1 - ... by region
# joao | v0101   Midwest    North Northeast    South Southwest
# joao | 2008   10667378 11126968  39153609 21586699  63678198
survey::svytable(~v0101+region, design=sample.pnad08.14y)

#Table 1 - ... by sex
# joao | v0101       Men      Women
# joao | 2008     70309203  75903649
survey::svytable(~v0101+v0302, design=sample.pnad08.14y)


#Tabela 2 da Knuth et al (2011), pratica de transporte ativo >30min both ways
#Region
# joao | factor(v1411 > 3)   Midwest     North Northeast     South Southwest
# joao | FALSE             1380105.8 1850873.3 6867758.7 3816272.7 7317537.3
# joao | TRUE               463071.8  826375.9 3629150.7 1260328.9 3348780.9
survey::svytable(~factor(v1411>3)+region, design=sample.pnad08.14y)

# joao |              region factor(v1411 > 3)      ci_l      ci_u
# joao | Midwest     Midwest         0.2512356 0.2309066 0.2727197
# joao | North         North         0.3086660 0.2752350 0.3442291
# joao | Northeast Northeast         0.3457352 0.3278847 0.3640311
# joao | South         South         0.2482623 0.2282011 0.2694714
# joao | Southwest Southwest         0.3139585 0.2979659 0.3304054
survey::svyby(~factor( v1411>3 ) ,
      ~region ,  
      design = sample.pnad08.14y,  
      vartype="ci",  level = 0.95,  svyciprop)


#Sexo
# joao | v0302
# joao | factor(v1411 > 3)      Men    Women
# joao | FALSE 11932424  9300124
# joao | TRUE   6004313  3523395
survey::svytable(~factor(v1411>3)+v0302, design=sample.pnad08.14y)

# joao |       v0302 factor(v1411 > 3)      ci_l      ci_u
# joao | Men     Men         0.3347495 0.3237936 0.3458864
# joao | Women Women         0.2747604 0.2645741 0.2851868
survey::svyby(~factor( v1411>3 ) ,
      ~v0302 ,  
      design = sample.pnad08.14y,  
      vartype="ci",  level = 0.95,  svyciprop
)

#Age
#
# joao |         AGE factor(v1411 > 3)      ci_l      ci_u
# joao | 0-17   0-17         0.2484907 0.2306645 0.2672159
# joao | 18-24 18-24         0.3053838 0.2906043 0.3205754
# joao | 25-34 25-34         0.3221246 0.3100338 0.3344585
# joao | 35-44 35-44         0.3206901 0.3083417 0.3332947
# joao | 45-54 45-54         0.3205290 0.3070800 0.3342829
# joao | 55-64 55-64         0.2982007 0.2821761 0.3147363
# joao | 65+     65+         0.2621073 0.2404058 0.2850327
survey::svyby(
  ~factor( v1411>3 ) ,
  ~AGE ,  
  design = sample.pnad08.14y,  
  vartype="ci",  level = 0.95,  svyciprop
)

