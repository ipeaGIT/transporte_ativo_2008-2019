
# PNAD----

# This eliminates observations with missing values in the weight variable
pnad2008dom <- pnad2008dom[!is.na(v4611),]

options( survey.lonely.psu = "adjust" )
sample.pnad08dom <- survey::svydesign(data = pnad2008dom,
                                      id = ~v4618, #PSU
                                      strata = ~v4617, #Strat
                                      weights = ~v4611, #household weight
                                      nest = TRUE)


# Check PNAD08 Results IBGE report
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



# check Household results
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



# Check Pnad2008 Results Knuth (2011)


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


# PNS13  ----------------
# Check against Published Report - ftp://ftp.ibge.gov.br/PNS/2013/pns2013.pdf

# Total population of Brazil above 18 years old (146.3 million)
# svytotal (~vcount , sample_pns_18y)

# Check against Gráfico 1, % of ind. above 18 years old whith good+very good 
# health (self-assessment)
# rafa | ok - Brasil 66.1%
# joao | ok - Brasil 66.19%
survey::svymean(~factor(N001<3), design = sample_pns_18y) # 66.1%

# rafa | ok - Regioes Nordeste 56.7% , Sul 69.5% , Sudeste 71.5, Norte 59.8%
# joao | ok - Regioes Nordeste 56.65% , Sul 69.63% , Sudeste 71.55, Norte 59.70%
survey::svyby(~factor(N001<3), ~region, design = sample_pns_18y
              ,  vartype="ci",  level = 0.95,  svyciprop)


# Active Commute time
# Confidence intervals for proportions
survey::svyciprop(~actv_commutetime_10to19, design = sample_pns_18y
                  , method = c("likelihood"), level = 0.95)
survey::svyciprop(~actv_commutetime_10to19, design = sample_pns_18y
                  , method = c("likelihood"), level = 0.95)
survey::svyciprop(~actv_commutetime_20to29, design = sample_pns_18y
                  , method = c("likelihood"), level = 0.95)
survey::svyciprop(~actv_commutetime_30to44, design = sample_pns_18y
                  , method = c("likelihood"), level = 0.95)
survey::svyciprop(~actv_commutetime_45to59, design = sample_pns_18y
                  , method = c("likelihood"), level = 0.95)
survey::svyciprop(~actv_commutetime_from60, design = sample_pns_18y
                  , method = c("likelihood"), level = 0.95)

# Survey statistics on subsets
# Region
survey::svyby(formula = ~actv_commutetime_30to44,by = ~region
              , design = sample_pns_18y
              ,  vartype="ci",  level = 0.95,  svyciprop)

# Region and Sex
survey::svyby(~actv_commutetime_30to44, ~region+sexo, design = sample_pns_18y
              ,  vartype="ci",  level = 0.95,  svyciprop)

# Region and Sex
survey::svyby(~actv_commutetime_30to44, ~interaction(AGE,vehicleOwnership)
              , design = sample_pns_18y
              ,  vartype="ci",  level = 0.95,  svyciprop)

# Tabela 18.2 - Rendimento mensal médio habitual de todos os trabalhos de 18 anos 
# ou mais de idade que possui regime de trabalho não noturno, por sexo, com 
# indicação do intervalo de confiança de 95%, segundo as Grandes Regiões - 2013

svyby(~E01602, ~region+v0302, 
      design= subset(sample.pns13.pos, M005==2),  # no night shift
      vartype="ci",  level = 0.95,  na.rm = T, svymean)


# RESULTS----


# 4.1) survey_18y+_urban > prop_active_commute_30min ~ regioes ---- 

# Contingency tables for survey data
tab1 <- survey::svytable(~actv_commutetime_from30+region
                         , design = sample_pns_18y_urban)

tab1 <- as.data.frame(tab1) %>% data.table::as.data.table()
tab1[, Prop := Freq/sum(Freq),by = region]
tab1[, Prop := round(100 * Prop,1)]
tab1[, Source := "PNS"]
tab1[, Year := 2013]

# Fig.0 - Vehicle ownership, 2008 and 2013 ------------

#Contingency Table
svytable(~v2032+urban, design = sample.pnad08dom)
svytable(~v2032+urban, design = sample.pns13dom)

# PNAD tab_fig1
# Get General Proportions
table <- svyby(~v2032,~urban, sample.pnad08dom, svymean, vartype="ci", level = 0.95, na.rm=T)
# Reshape data frame
table <- gather(table,"urban",n,2:13)
table[,2] <- gsub("v2032", "", table[,2]) #rename collums
# Slice table to re-organize it
obs <- table[1:8,]
ci_l <- table[9:16,3]; ci_l <- as.data.frame(ci_l)
ci_u <- table[17:24,3]; ci_u <- as.data.frame(ci_u)
# Get table binding all the slices
tab_fig1pnad <- bind_cols(obs, ci_l,ci_u)
colnames(tab_fig1pnad)[2] <- "vehicle"

# PNS table 20
# Get General Proportions
table <- svyby(~v2032,~urban, sample.pns13dom, svymean, vartype="ci", level = 0.95, na.rm=T)
# Reshape data frame
table <- gather(table,"urban",n,2:13)
table[,2] <- gsub("v2032", "", table[,2]) #rename collums
# Slice table to re-organize it
obs <- table[1:8,]
ci_l <- table[9:16,3]; ci_l <- as.data.frame(ci_l)
ci_u <- table[17:24,3]; ci_u <- as.data.frame(ci_u)
# Get table binding all the slices
tab_fig1pns <- bind_cols(obs, ci_l,ci_u)
colnames(tab_fig1pns)[2] <- "vehicle"


#add var year to table
tab_fig1pnad$year <- 2008; tab_fig1pns$year <- 2013

#Bind tables from each dataset into one single table 1
tab_fig1 <- bind_rows(tab_fig1pnad, tab_fig1pns)
colnames(tab_fig1)[3] <- "Proportion"
remove(tab_fig1pnad, tab_fig1pns, ci_l, ci_u,obs,table)


# Create chart 
fig1 <-  
  ggplot(tab_fig1, aes(x=factor(year), y=Proportion, fill=vehicle)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_manual(values = wes_palette("Chevalier")) +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Year",y="Proportion") +
  scale_y_continuous(labels=percent) +
  facet_grid(. ~ urban) +
  baseplot +
  theme(legend.title=element_blank()) 


# save chart       
ggsave(fig1, file="./plots/fig1 Motorization urban-rural.png", dpi = 800,
       width = 20, height = 15, units = "cm")















# Fig 1 Pop characteristics vs Proportion of Vehicle Ownership 2013 ------------


tab1_year <- svyby(~factor( dummyVehicle=="Yes") ,
                   ~quintileBR+year, svyciprop ,
                   design = sample.pns13.18y ,
                   vartype="ci", level = 0.95) %>% setDT()


tab1_sex <- svyby(~factor( dummyVehicle=="Yes") ,
                  ~quintileBR+v0302, svyciprop ,
                  design = sample.pns13.18y ,
                  vartype="ci", level = 0.95) %>% setDT()


tab1_edu <- svyby(~factor( dummyVehicle=="Yes" ) , 
                  ~quintileBR+edugroup, svyciprop , # edugroup v4745
                  design = sample.pns13.18y ,
                  vartype="ci", level = 0.95) %>% setDT()


tab1_age <- svyby(~factor( dummyVehicle=="Yes") ,
                  ~quintileBR+AGE, svyciprop ,
                  design = sample.pns13.18y ,
                  vartype="ci", level = 0.95) %>% setDT()


tab1_actvcommute <- svyby(~factor( dummyVehicle=="Yes" ) ,
                          ~quintileBR+P040, svyciprop ,
                          design = sample.pns13.18y ,
                          vartype="ci", level = 0.95) %>% setDT()


tab1_year$var <- "Year"
tab1_sex$var <- "Sex"
tab1_age$var <- "Age"
tab1_edu$var <- "Education"
tab1_actvcommute$var <- "Active Commute"

beep() # beep alert

# tab1_region <- svyby(~factor( dummyVehicle=="Yes" ) ,
#                      ~region+urban, svyciprop ,
#                      design = sample.pns13.18y ,
#                      vartype="ci", level = 0.95) %>% setDT()
# tab1_region$var <- "Region"

# tab1_metro <- svyby(~factor( v2032!="None" ) ,
#                      ~metro+urban, svyciprop ,
#                      design = sample.pns13.18y ,
#                      vartype="ci", level = 0.95) %>% setDT()
# tab1_metro$var <- "Metropolitan Area"


# get all subtables into a list
#tab1_list <- list(tab1_actvcommute, tab1_age, tab1_edu, tab1_sex, tab1_year)
tab1_list <- list(tab1_year, tab1_sex, tab1_edu, tab1_age, tab1_actvcommute)

# change name of sub-tables' columns so we can stack them up
tab1_list <- lapply(tab1_list, setNames, c("quintileBR", "value","Proportion", "ci_l", "ci_u", "var"))

# stack sub-tables from a list into a single data.frame
tab1 <- rbindlist(tab1_list)



#### Reorder values and labels

# reorder factor levels by poportion values
tab1$value <- reorder(tab1$value, tab1$Proportion)

# add labels to income variable
tab1[, quintileBR := factor(quintileBR, levels=c(1:5), labels = c("Q1 (poorest)","Q2","Q3","Q4","Q5 (richest)"))]


# Manually reorder Age      
tab1$value <- relevel(tab1$value, "65+")
tab1$value <- relevel(tab1$value, "55-64")
tab1$value <- relevel(tab1$value, "45-54")
tab1$value <- relevel(tab1$value, "35-44")
tab1$value <- relevel(tab1$value, "25-34")
tab1$value <- relevel(tab1$value, "18-24")

# Manually reorder Active commute      
tab1$value <- relevel(tab1$value, "Yes, all the journey")
tab1$value <- relevel(tab1$value, "Yes, part of the journey")
tab1$value <- relevel(tab1$value, "No")

# change order or plot facets
tab1[, var := factor(var, levels=c( "Year", "Sex", "Education", "Age", "Active Commute"))]

# save table 1
saveRDS(tab1, file="tab1.Rds")


### ONE SINGLE PLOT

fig1 <- 
  ggplot(data=tab1) +
  geom_point( aes(y=value, x=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  geom_errorbarh( aes(y=value, x=Proportion, xmin=ci_u, xmax=ci_l, color=quintileBR),  size=0.5, height = 0) + 
  facet_grid( var~., scales = "free_y", space="free") + 
  baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  # colored chart         
  # scale_colour_brewer(palette = "Greys") + scale_fill_brewer(palette = "Greys")
  # scale_fill_viridis(discrete=T) +  scale_color_viridis(discrete=T)
  # scale_fill_manual(values = wes_palette("Royal1"), guide = guide_legend(title = NULL)) +
  # scale_color_manual(values = wes_palette("Royal1"), guide = guide_legend(title = NULL)) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title.y  = element_blank()) +
  theme(legend.position = "top") 
# ggtitle('Fig. 2 Vehicle ownership in the household in urban and rural areas. Brazil, 2013') 



ggsave(fig1, file="./plots/fig1.png", dpi = 800,
       width = 20, height = 20, units = "cm")







###### Table 2- Pop characteristics- vehicle 2013 vs 2008 ------------------------


# PNS subtables
# tab2_pns_total <- svyby(~factor( dummyVehicle=="Yes") ,
#                           ~quintileBR+year, svyciprop ,
#                           design = sample.pns13.18y ,
#                           vartype="ci", level = 0.95) %>% setDT()
# 
# tab2_pns_urban <- svyby(~factor( dummyVehicle=="Yes") ,
#                          ~quintileBR+urban+year, svyciprop ,
#                          design = sample.pns13.18y ,
#                          vartype="ci", level = 0.95) %>% setDT()
# 
# tab2_pns_region <- svyby(~factor( dummyVehicle=="Yes") ,
#                           ~quintileRegion+region+year, svyciprop ,
#                           design = sample.pns13.18y ,
#                           vartype="ci", level = 0.95) %>% setDT()
# 
# tab2_pns_metro <- svyby(~factor( dummyVehicle=="Yes") ,
#                         ~quintileMetro+metro+year, svyciprop ,
#                         design = sample.pns13.18y ,
#                         vartype="ci", level = 0.95) %>% setDT()

tab2_pns_total <- svyby(~ dummyVehicle=="Yes" ,
                        ~quintileBR+year, svymean ,
                        design = sample.pns13.18y ,
                        vartype="ci", level = 0.95, na.rm=T) %>% setDT()

tab2_pns_urban <- svyby(~ dummyVehicle=="Yes" ,
                        ~quintileBR+urban+year, svymean ,
                        design = sample.pns13.18y ,
                        vartype="ci", level = 0.95, na.rm=T) %>% setDT()


tab2_pns_region <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileRegion+region+year, svymean ,
                         design = sample.pns13.18y ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()

tab2_pns_metro <- svyby(~ dummyVehicle=="Yes" ,
                        ~quintileMetro+metro+year, svymean ,
                        design = sample.pns13.18y ,
                        vartype="ci", level = 0.95, na.rm=T) %>% setDT()


# PNAD subtables
tab2_pnad_total <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileBR+year, svymean ,
                         design = sample.pnad08.18y ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()

tab2_pnad_urban <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileBR+urban+year, svymean ,
                         design = sample.pnad08.18y ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()


tab2_pnad_region <- svyby(~ dummyVehicle=="Yes" ,
                          ~quintileRegion+region+year, svymean ,
                          design = sample.pnad08.18y ,
                          vartype="ci", level = 0.95, na.rm=T) %>% setDT()

tab2_pnad_metro <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileMetro+metro+year, svymean ,
                         design = sample.pnad08.18y ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()


tab2_pns_total$var <- "Total"
tab2_pns_urban$var <- "Urban"
tab2_pns_region$var <- "Region"
tab2_pns_metro$var <- "Metropolitan Area"
tab2_pnad_total$var <- "Total"
tab2_pnad_urban$var <- "Urban"
tab2_pnad_region$var <- "Region"
tab2_pnad_metro$var <- "Metropolitan Area"
beep() # beep alert


# organize total subtables
tab2_total <- rbind(tab2_pns_total, tab2_pnad_total)
tab2_total <- tab2_total[, c(1,2,4,6,8,9), with=FALSE]
colnames(tab2_total) <- c("quintileBR", "year", "Proportion", "ci_l", "ci_u" , "var")
tab2_total$value <- "Brazil"
setcolorder(tab2_total, neworder = c("quintileBR", "value", "year", "Proportion", "ci_l", "ci_u", "var") )

# get all subtables into a list
tab2_list <- list(tab2_pns_urban, tab2_pns_region, tab2_pns_metro, tab2_pnad_urban, tab2_pnad_region, tab2_pnad_metro)

#keep selected columns
tab2_list <- lapply(tab2_list, function(x) { x <- x[, c(1,2,3,5,7,9,10), with=FALSE] } )

# change name of sub-tables' columns so we can stack them up
tab2_list <- lapply(tab2_list, setNames, c("quintileBR", "value", "year","Proportion", "ci_l", "ci_u", "var"))

# stack all sub-tables into a single data.frame
tab2 <- rbindlist(tab2_list)
tab2 <- rbind(tab2, tab2_total)

# # remove subtables 2
# rm(list=ls(pattern="^tab2_"))


#### Reorder values and labels

# reorder factor levels by poportion values
tab2$value <- reorder(tab2$value, tab2$Proportion)

# add labels to income variable
tab2[, quintileBR := factor(quintileBR, levels=c(1:5), labels = c("Q1 (poorest)","Q2","Q3","Q4","Q5 (richest)"))]

# change order or plot facets
tab2[, var := factor(var, levels=c( "Total", "Urban", "Region", "Metropolitan Area"))]
tab2[, year := factor(year, levels=c( 2008, 2013))]


# save table 2
saveRDS(tab2, file="tab2.Rds")

###### Fig 2- Pop characteristics- vehicle 2013 vs 2008 ------------------------

tab2 <- readRDS("tab2.Rds")



### ONE SINGLE PLOT

fig2 <- 
  ggplot(data=tab2) +
  geom_point( aes(y=as.factor(year), x=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  #geom_errorbarh( aes(y=value, x=Proportion, xmin=ci_u, xmax=ci_l, color=quintileBR),  size=0.5, height = 0) + 
  #facet_grid( value~., scales = "free_y", space="free") + 
  facet_grid(var+value ~., scales = "free_y", space="free") 
baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  
  
  # colored chart         
  # scale_colour_brewer(palette = "Greys") + scale_fill_brewer(palette = "Greys")
  # scale_fill_viridis(discrete=T) +  scale_color_viridis(discrete=T)
  # scale_fill_manual(values = wes_palette("Royal1"), guide = guide_legend(title = NULL)) +
  # scale_color_manual(values = wes_palette("Royal1"), guide = guide_legend(title = NULL)) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title.y  = element_blank()) +
  theme(legend.position = "top") 
# ggtitle('Fig. 2 Vehicle ownership in the household in urban and rural areas. Brazil, 2013') 



ggsave(fig2, file="./plots/fig2.png", dpi = 800,
       width = 20, height = 20, units = "cm")
_____________
_____________
_____________



# main plot
mainplot <- 
  ggplot(tab2, aes(x = Proportion, y = factor(year), color=quintileBR, fill=quintileBR)) +
  geom_point(size=3, shape=21) +
  geom_errorbarh( aes(x=Proportion, y=factor(year), xmin=ci_u, xmax=ci_l, color=quintileBR),  size=0.5, height = 0) + 
  facet_grid(var+value ~., scales = "free_y", space="free", labeller = label_bquote(rows = .(var))) +
  baseplot +
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = seq(0,1, 0.1), labels = scales::percent) +
  theme(axis.title.y = element_blank()) 
# + theme(legend.position = "top") 

# plot labels
lbls <- 
  ggplot(tab2, aes(x = 0, y = factor(year))) +
  geom_point(color = NA) +
  geom_text(aes(x = 0, y = 1, label = value), color = 'black' ) +
  scale_x_continuous(limits = c(0,0), breaks = 0) +
  facet_grid(var+value ~.) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(color = NA),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none') 

# arrange plot and save
fig2 <- grid.arrange(lbls, mainplot, ncol = 2, widths = c(1,9))

ggsave(fig2, file="./plots/fig2.png", dpi = 800,
       width = 45, height = 25, units = "cm")




###### Fig 2- Pop characteristics- vehicle 2013 ------------------------

# reorder table
tab1 <-  tab1[order(var,value,-rank(Proportion))]

# get totals and paste in Excel
tab1_totals <- svytable(~urban, design = sample.pnad08dom)
write.table(tab1_totals, "clipboard", sep="\t", row.names=FALSE)

# TABELA  
# Subset into two tables: urban and rural
tab1_urban <- tab1[urban=="Urban"]
tab1_rural <- tab1[urban=="Rural"]

# Bind them side by side
tab1_table <- cbind(tab1_urban, tab1_rural)

# coppy table to clipboard
write.table(tab1_table, "clipboard", sep="\t", row.names=FALSE)
# Now you paste it in a Excel spread sheet





#### Fig 2.1 Motorcycle Ownership 2013. ------------------------

tab2_sex <- svyby(~factor( A01817==1 ) ,
                  ~v0302+urban, svyciprop ,
                  design = sample.pns13.18y ,
                  vartype="ci", level = 0.95) %>% setDT()
tab2_sex$var <- "Sex"


tab2_edu <- svyby(~factor( A01817==1 ) ,
                  ~v4745+urban, svyciprop ,
                  design = sample.pns13.18y ,
                  vartype="ci", level = 0.95) %>% setDT()
tab2_edu$var <- "Education"

tab2_age <- svyby(~factor( A01817==1 ) ,
                  ~AGE+urban, svyciprop ,
                  design = sample.pns13.18y ,
                  vartype="ci", level = 0.95) %>% setDT()
tab2_age$var <- "Age"


tab2_region <- svyby(~factor( A01817==1 ) ,
                     ~region+urban, svyciprop ,
                     design = sample.pns13.18y ,
                     vartype="ci", level = 0.95) %>% setDT()
tab2_region$var <- "Region"

# tab2_metro <- svyby(~factor( A01817==1 ) ,
#                      ~metro+urban, svyciprop ,
#                      design = sample.pns13.18y ,
#                      vartype="ci", level = 0.95) %>% setDT()
# tab2_metro$var <- "Metropolitan Area"


# get all subtables into a list
tab2_list <- list(tab2_region, tab2_sex, tab2_edu, tab2_age)



# change name of columns so we can stack them
lapply( list(tab2_region, tab2_sex, tab2_edu, tab2_age) , 
        function(x) {
          colnames(x)[c(1,3)] <- c("value", "Proportion")
          #x$value <- reorder(x$value, x$Proportion)
        })

# stack sub-tables from a list into a single data.frame
tab2 <- rbind.fill(tab2_list) %>% setDT()


# reorder factor levels by poportion values
tab2$value <- reorder(tab2$value, tab2$Proportion)
# Manually reorder Age      
tab2$value <- relevel(tab2$value, "65+")
tab2$value <- relevel(tab2$value, "55-64")
tab2$value <- relevel(tab2$value, "45-54")
tab2$value <- relevel(tab2$value, "35-44")
tab2$value <- relevel(tab2$value, "25-34")
tab2$value <- relevel(tab2$value, "18-24")


# ONE SINGLE PLOT

fig2motorcycle <- ggplot(data=tab2) +
  geom_point( aes(y=value, x=Proportion, color=urban, fill=urban), size=3, shape=21 ) +
  geom_errorbarh( aes(y=value, x=Proportion, xmin=ci_u, xmax=ci_l, color=urban),  size=0.5, height = 0) + 
  facet_grid( var~., scales = "free_y") +
  baseplot +
  scale_fill_manual(values = wes_palette("Royal1")) +
  scale_color_manual(values = wes_palette("Royal1")) +
  ggtitle('Motorcycle ownership in the household in urban and rural areas. Brazil, 2013') 

ggsave(fig2motorcycle, file="./plots/fig2motorcycleo.png", dpi = 800,
       width = 20, height = 20, units = "cm")



# clean objects and memory
rm(list=setdiff(ls(), c("baseplot", "sample.pns13dom", "sample.pnad08dom", "sample.pns13.18y", "sample.pnad08.18y")))
gc(reset = T)







# Fig 3 Health characteristics of study sample  ------------


!!!!!!!!!!!! 
  !!!!!!!!!!!! table(pns2013$N001) # health perception
!!!!!!!!!!!! 
  
  tab3.1_diabetes <- svyby(~factor( Q030==1 ) ,
                           ~dummyVehicle+urban, svyciprop ,
                           design = sample.pns13.18y ,
                           vartype="ci", level = 0.95) %>% setDT()

tab3.1_hypertension  <- svyby(~factor( Q002==1 ) ,
                              ~dummyVehicle+urban, svyciprop ,
                              design = sample.pns13.18y ,
                              vartype="ci", level = 0.95) %>% setDT()

tab3.1_cholesterol  <- svyby(~factor( Q060==1 ) ,
                             ~dummyVehicle+urban, svyciprop ,
                             design = sample.pns13.18y ,
                             vartype="ci", level = 0.95) %>% setDT()

tab3.1_diabetes$var <- "Diabetes"
tab3.1_hypertension$var <- "Hypertension"
tab3.1_cholesterol$var <- "High Cholesterol"


# get subtables into a list
tab3.1_list <- list(tab3.1_diabetes, tab3.1_hypertension, tab3.1_cholesterol)

# change name of columns so we can stack them
lapply( tab3.1_list, function(x) { colnames(x)[c(1,3)] <- c("value", "Proportion") })

# stack sub-tables from a list into a single data.frame
tab3.1 <- rbind.fill(tab3.1_list) %>% setDT()


fig3.1 <-  
  ggplot(data=tab3.1) +
  geom_point( aes(y=urban, x=Proportion, color=value, fill=value), size=3, shape=21 ) +
  geom_errorbarh( aes(y=urban, x=Proportion, xmin=ci_u, xmax=ci_l, color=value),  size=0.5, height = 0) + 
  facet_grid( var~. ) + #scales = "free"
  baseplot +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title.y  = element_blank()) +
  scale_fill_manual(values = wes_palette("Royal1"), guide = guide_legend(title = "Vehicle ownership")) +
  scale_color_manual(values = wes_palette("Royal1"), guide = guide_legend(title = "Vehicle ownership")) +
  theme(legend.position = "top") +
  
  # scale_fill_grey() +
  # scale_color_grey() +
  #ggtitle("Veihcle ownership in the household by respondent's characteristics. Brazil, 2013") +
  theme(strip.text.x = element_text(size = 8))





# Fig 3.1   Other health outcomes

tab3.2_bmi  <- svyby(~ bmi, 
                     ~dummyVehicle+urban,
                     design = sample.pns13.18y ,
                     svymean, na.rm=T,
                     vartype="ci", level = 0.95) %>% setDT()


tab3.2_bmi$var <- "BMI"


# get subtables into a list
tab3.2_list <- list(tab3.2_bmi)

# change name of columns so we can stack them
lapply( tab3.2_list, function(x) { colnames(x)[c(1,3)] <- c("value", "Proportion") })

# stack sub-tables from a list into a single data.frame
tab3.2 <- rbind.fill(tab3.2_list) %>% setDT()





fig3.2 <- 
  ggplot(data=tab3.2) +
  geom_point( aes(y=urban, x=Proportion, color=value, fill=value), size=3, shape=21 ) +
  geom_errorbarh( aes(y=urban, x=Proportion, xmin=ci_u, xmax=ci_l, color=value),  size=0.5, height = 0) + 
  facet_grid( var~., scales = "free_x" ) +
  baseplot +
  scale_fill_manual(values = wes_palette("Royal1"), guide = guide_legend(title = "Vehicle ownership")) +
  scale_color_manual(values = wes_palette("Royal1"), guide = guide_legend(title = "Vehicle ownership")) +
  theme(axis.title.y  = element_blank()) +
  scale_x_continuous(limits = c(22, 28)) +
  theme(legend.position="none") +
  
  xlab( expression(paste("BMI (Kg/", m^{2},')')))




# join maps   
# fig3_health <- grid.arrange(fig3.1, fig3.2, nrow=2, ncol=1) 
# fig3_health <- plot_grid(fig3.1, fig3.2, nrow = 2, ncol = 1) # labels=c("A", "B")

#draw_plot(plot, x = 0, y = 0, width = 1, height = 1)
fig3_health <- ggdraw() + draw_plot(fig3.1, 0, .35, 1, .65) +
  draw_plot(fig3.2, 0,   0, .98, .3)

ggsave(fig3_health, file="./plots/fig3_health.png", dpi = 100,
       width = 20, height = 20, units = "cm")







----------------------------------------------------------------------
  
  
  
  
  # Average weight, car access and active travel
  test  <- svyby(~ P00101, 
                 ~v2032+urban+P040,
                 design = sample.pns13.18y ,
                 svymean, na.rm=T,
                 vartype="ci", level = 0.95) %>% setDT()

head(test)
colnames(test)[4] <- "Proportion" 

ggplot(data= test) +
  geom_point( aes(y=P040, x=Proportion, color=v2032, fill=v2032), size=3, shape=21 ) +
  geom_errorbarh( aes(y=P040, x=Proportion, xmin=ci_u, xmax=ci_l, color=v2032),  size=0.5, height = 0) + 
  facet_grid( .~urban) +
  baseplot +
  scale_fill_manual(values = wes_palette("Royal1")) +
  scale_color_manual(values = wes_palette("Royal1"))









# Table 4 - Proportion of active travel among workers by sex. Brazil, 2013. ------------

# tabulation
tab4 <-   ftable(svytable(~P040+v0302, design = sample.pns13.18y))
# get Proportions
table4 <- prop.table(tab4, margin=2)

# convert to data frame
table4 <- as.data.frame(as.matrix(table4))

# Get totals
table4["Total" ,] <- colSums(table4)

# multiply by 100
#table4 <- table4 *100

print(table4)

setDT(table4, keep.rownames = TRUE)[]

write.table(table4, "clipboard", sep="\t", row.names=FALSE)






##### Fig 4 - Proportion of active travel among workers by sex. Brazil, 2013. ------------


# actv_commutetime30,  actv_commutetime,  actv_traveltimehabacts,total_actvtraveltime, physicallyactive30
table(pns2013$P040)

tab4.1_actv_commutetime30 <- svyby(~actv_commutetime30==1  ,
                                   ~dummyVehicle+urban, svyciprop ,
                                   design = sample.pns13.18y ,
                                   vartype="ci", level = 0.95) %>% setDT()

tab4.2_actv_commutetime  <- svyby(~actv_commutetime, 
                                  ~dummyVehicle+urban,
                                  design = sample.pns13.18y ,
                                  svymean, na.rm=T,
                                  vartype="ci", level = 0.95) %>% setDT()


colnames(tab4.1_actv_commutetime30)[3] <- "Proportion"
colnames(tab4.2_actv_commutetime)[3] <- "mean"

fig4.1 <- ggplot(data= tab4.1_actv_commutetime30) +
  geom_point( aes(y=urban, x=Proportion, color=dummyVehicle, fill=dummyVehicle), size=3, shape=21 ) +
  geom_errorbarh( aes(y=urban, x=Proportion, xmin=ci_u, xmax=ci_l, color=dummyVehicle),  size=0.5, height = 0) + 
  #facet_grid( var~. ) + #scales = "free"
  baseplot +
  scale_fill_manual(values = wes_palette("Royal1"), guide = guide_legend(title = "Vehicle 
ownership")) +
  scale_color_manual(values = wes_palette("Royal1"), guide = guide_legend(title = "Vehicle 
ownership")) +
  theme(legend.position="none") +
  scale_x_continuous(labels = scales::percent) + 
  theme(axis.title.y  = element_blank()) +
  labs(x = "Proportion of Active commute longer than 30 minutes")




fig4.2 <- ggplot(data= tab4.2_actv_commutetime) +
  geom_point( aes(y=urban, x=mean, color=dummyVehicle, fill=dummyVehicle), size=3, shape=21 ) +
  geom_errorbarh( aes(y=urban, x=mean, xmin=ci_u, xmax=ci_l, color=dummyVehicle),  size=0.5, height = 0) + 
  #facet_grid( var~. ) + #scales = "free"
  baseplot +
  scale_fill_manual(values = wes_palette("Royal1"), guide = guide_legend(title = "Vehicle 
ownership")) +
  scale_color_manual(values = wes_palette("Royal1"), guide = guide_legend(title = "Vehicle 
ownership")) +
  theme(legend.position="right") +
  theme(axis.title.y  = element_blank()) +
  labs(x = "Avg. active commute time (minutes)") 




# join maps   
# fig4_actv_commute <- grid.arrange(fig4.1, fig4.2, nrow=1, ncol=2) 
# 
# fig4_actv_commute <- plot_grid(fig4.1, fig4.2, nrow = 1, ncol = 2, labels=c("A", "B")) 
# 
# ggsave(fig4_actv_commute, file="./plots/fig4.png", dpi = 800,
#        width = 25, height = 10, units = "cm")

Fig4 <- plot_grid(fig4.1, fig4.2, labels = c("A", "B"))




ggsave(Fig4, file="./plots/Fig4.png", dpi = 100,
       width = 25, height = 10, units = "cm")

# #draw_plot(plot, x = 0, y = 0, width = 1, height = 1)
# 
# test <- ggdraw() + draw_plot(fig4.1, 0, 0, .5, 1) +
#   draw_plot(fig4.2, 0.5, 0, .5, 1) +  draw_plot_label(c("A","B"))









# BOX PLOT
ggplot(pns2013) +
  geom_boxplot( aes(x= P040 , y= P00101, color=v2032, weight=V0029 )) +
  facet_grid( urban~. ) +
  scale_colour_brewer(palette = "RdBu", name = "Income Decile")

svyboxplot(P00101~v2032, design = sample.pns13.18y )









###### OLD results ---------- 

#### Tabelas Geral do Brasil - HOMENS x Mulheres [tabela H.1 a H.7 e H.total, e repete para M.1 ...#### 


# Table 0 - Compatibility between Active Travel variables ------------

# % Active Commute
ftable(svytable(~V1410+V0302, design = sample.pnad08.18y)) #PNAD 2008
ftable(svytable(~P040+v0302, design = sample.pns13.18y)) #PNS 2013



# % Active Commute > 30 min
ftable(svytable(~actv_commutetime30+V0302, design = sample.pnad08.18y)) #PNAD 2008
ftable(svytable(~actv_commutetime30+V0302, design = sample.pns13.18y)) #PNS 2013


ftable(svytable(~actv_commutetime30+v0302+v1410, design = sample.pnad08.18y)) #PNAD 2008
ftable(svytable(~actv_commutetime30+v0302+P040, design = sample.pns13.18y)) #PNS 2013


tab0pnad <- svyby(~factor( V1410=="Yes" ) ,
                  ~V0302,
                  design = sample.pnad08.18y ,
                  vartype="ci",
                  level = 0.95,
                  svyciprop)

tab0pns <- svyby(~factor( P040=="Yes, all the journey" ) ,
                 ~v0302,
                 design = sample.pns13.18y ,
                 vartype="ci",
                 level = 0.95,
                 svyciprop)

tab0.1pns <- svyby(~factor( P040=="Yes, part of the journey" ) ,
                   ~v0302,
                   design = sample.pns13.18y ,
                   vartype="ci",
                   level = 0.95,
                   svyciprop)

#add var year to table
tab0pnad$year <- 2008; tab0pns$year <- 2013; tab0.1pns$year <- 2013
tab0pnad$response <- "Yes"; tab0pns$response <- "Yes, all the journey"; tab0.1pns$response <- "Yes, part of the journey"
colnames(tab0pnad)[2] <- "Proportion"
colnames(tab0pns)[2] <- "Proportion"
colnames(tab0.1pns)[2] <- "Proportion"


#Bind tables from each dataset into one single table 1
tab0 <- bind_rows(tab0pnad, tab0pns,tab0.1pns); remove(tab0pnad, tab0pns, tab0.1pns)

# Manual Adjustment postion of erro bar positions
tab0$ci_l[tab0$response == "Yes, part of the journey"] <- with(tab0,ci_l[response == "Yes, part of the journey"] +
                                                                 ci_l[response == "Yes, all the journey"])
tab0$ci_u[tab0$response == "Yes, part of the journey"] <- with(tab0,ci_u[response == "Yes, part of the journey"] +
                                                                 ci_u[response == "Yes, all the journey"])


graf0 <- ggplot(tab0, aes(x=factor(year), y=Proportion, fill=response)) + 
  geom_bar(position="stack", stat="identity") +
  facet_grid(. ~ v0302) +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u),
                width=.2,                    # Width of the error bars
                position=position_dodge(0))+
  scale_y_continuous(limits=c(0, 0.5))+
  baseplot +
  theme(legend.position="bottom")



ggsave(graf0, file="graf0 Act Travel Sex.jpeg" )



# Table0.30min - % of Active Travel > 30 min by Sex  -------------

#PNS
#Crete separated tables for men and women
y <- subset( sample.pns13.18y , v0302 == 'Women' )
tab030pnswomen <- svymean(~factor(interaction(P040, actv_commutetime30==1 ) ), na.rm=TRUE, y)
y <- subset( sample.pns13.18y , v0302 == 'Men' )
tab030pnsmen <- svymean(~factor(interaction(P040, actv_commutetime30==1 ) ), na.rm=TRUE, y)
# Convert to data frame
tab030pnswomen <- as.data.frame(tab030pnswomen); 
tab030pnsmen <- as.data.frame(tab030pnsmen); 

#Bind tab030pns for women and men
tab030pns <- bind_rows(tab030pnswomen, tab030pnsmen)

#Add variables to tabpns030
tab030pns$year <- 2013
tab030pns$v0302 <- c("Women","Women","Women","Women","Women","Men","Men","Men","Men","Men")
tab030pns$actv_commutetime30 <- c(0,0,0,1,1,0,0,0,1,1)
tab030pns$response <- c("No","Yes, all the journey","Yes, part of the journey","Yes, all the journey","Yes, part of the journey","No","Yes, all the journey","Yes, part of the journey","Yes, all the journey","Yes, part of the journey")



#PNAD
#Crete separated tables for men and women
y <- subset( sample.pnad08.18y , v0302 == 'Women' )
tab030pnadwomen <- svymean(~factor(interaction(v1410, actv_commutetime30==1 ) ), na.rm=TRUE, y)
y <- subset( sample.pnad08.18y , v0302 == 'Men' )
tab030pnadmen <- svymean(~factor(interaction(v1410, actv_commutetime30==1 ) ), na.rm=TRUE, y)
# Convert to data frame
tab030pnadwomen <- as.data.frame(tab030pnadwomen); 
tab030pnadmen <- as.data.frame(tab030pnadmen); 

#Bind tab030pnad for women and men
tab030pnad <- bind_rows(tab030pnadwomen, tab030pnadmen)

#Add variables tab030pnad
tab030pnad$year <- 2008
tab030pnad$v0302 <- c("Women","Women","Women","Men","Men","Men")
tab030pnad$actv_commutetime30 <- c(0,0,1,0,0,1)
tab030pnad$response <- c("No","Yes", "Yes","No","Yes", "Yes")


#Bind tables from each dataset into one single table 1
tab030 <- bind_rows(tab030pnad, tab030pns)
tab030 <- subset(tab030, actv_commutetime30==1) # Keep only active commuters >30min

#Change var names and add confidence intervals of 95% (+-2 standard deviations)
colnames(tab030)[1] <- "Proportion"
tab030$ci_l <- tab030$Proportion - 2*tab030$SE
tab030$ci_u <- tab030$Proportion + 2*tab030$SE
remove(tab030pnad,tab030pns,tab030pnswomen,tab030pnsmen,tab030pnadwomen,tab030pnadmen,y) #removed unsued objects


# Manual Adjustment postion of erro bar positions
tab030$ci_l[tab030$response == "Yes, part of the journey"] <- with(tab030,ci_l[response == "Yes, part of the journey"] +
                                                                     ci_l[response == "Yes, all the journey"])
tab030$ci_u[tab030$response == "Yes, part of the journey"] <- with(tab030,ci_u[response == "Yes, part of the journey"] +
                                                                     ci_u[response == "Yes, all the journey"])

#Create Chart
graf01.30 <- ggplot(tab030, aes(x=factor(year), y=Proportion, fill=response)) + 
  geom_bar(position="stack", stat="identity") +
  facet_grid(. ~ v0302) +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u),
                width=.2,                    # Width of the error bars
                position=position_dodge(0))+
  scale_y_continuous(limits=c(0, 0.5))+
  baseplot +
  theme(legend.position="bottom")




#save plot
ggsave(graf01.30, file="graf01.30 Act Travel 30min Sex.jpeg" )




# Group plots graf0 and graf0.130
GRAF0 <-  plot_grid(graf0, graf01.30, labels = c("Active Commute", "More than 30 min"))
ggsave(GRAF0, file="GRAF0.jpeg" )






# Table1 - % of Active Travel by Sex and Urban vs Rural -------------

#Contingency Table
svytable(~v1410+v0302+urban, design = sample.pnad08.18y)

tab1pnad <- svyby(~v1410=="Yes"  ,
                  ~urban + v0302,
                  design = sample.pnad08.18y ,
                  vartype="ci",
                  level = 0.95,
                  svyciprop
)

tab1pns <- svyby(~v1410=="Yes"  ,
                 ~urban + v0302,
                 design = sample.pns13.18y ,
                 vartype="ci",
                 level = 0.95,
                 svyciprop)


#add var year to table
tab1pnad$year <- 2008; tab1pns$year <- 2013

#Bind tables from each dataset into one single table 1
tab1 <- bind_rows(tab1pnad, tab1pns); colnames(tab1)[3] <- "Proportion"; remove(tab1pnad, tab1pns)

graf1 <- ggplot(tab1, aes(x=v0302, y=Proportion, fill=urban)) + 
  facet_grid(. ~ year) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u),
                width=.2,                    # Width of the error bars
                position=position_dodge(0.9)) +
  #scale_y_continuous(limits=c(0, 0.5)) +
  baseplot

graf1.1 <- ggplot(tab1, aes(x=factor(year), y=Proportion, fill=urban)) + 
  facet_grid(. ~ v0302) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  baseplot

ggsave(graf1, file="graf1 Urb-Rural Sex.jpeg" )
ggsave(graf1.1, file="graf1.1 Urb-Rural Sex.jpeg" )








gc()
# Table2 - % of Active Travel by Sex and Education -------------
# chart examples http://sape.inf.usi.ch/quick-reference/ggplot2/geom_errorbarh

tab2pnad <- svyby(~v1410=="Yes" ,
                  ~v4745 + v0302,
                  design = sample.pnad08.18y ,
                  vartype="ci",
                  level = 0.95,
                  svyciprop
)


tab2pns <- svyby(~ v1410=="Yes" ,
                 ~v4745 + v0302,
                 design = sample.pns13.18y ,
                 vartype="ci",
                 level = 0.95,
                 svyciprop
)


#add var year to table
tab2pnad$year <- 2008; tab2pns$year <- 2013

#Bind tables from each dataset into one single table
tab2 <- bind_rows(tab2pnad, tab2pns); colnames(tab2)[3] <- "Proportion"; remove(tab2pnad, tab2pns)
# Reorder values - https://kohske.wordpress.com/2010/12/29/faq-how-to-order-the-factor-variables-in-ggplot2/
tab2$v4745 <- reorder(tab2$v4745, tab2$Proportion)

graf2 <-  ggplot() + 
  geom_errorbarh(data=tab2, mapping=aes(y=v4745, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(year)), height=0.2, size=1) + 
  geom_point(data=tab2, mapping=aes(y=v4745, x=Proportion, color=factor(year)), size=3, shape=21 ) +
  facet_grid(. ~ v0302) +
  baseplot


graf2.1 <-  ggplot() + 
  geom_errorbarh(data=tab2, mapping=aes(y=v4745, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(v0302)), height=0.2, size=1) + 
  geom_point(data=tab2, mapping=aes(y=v4745, x=Proportion, color=factor(v0302)), size=3, shape=21 ) +
  facet_grid(. ~ year) +
  baseplot

ggsave(graf2, file="graf2 Education-Sex.jpeg" )
ggsave(graf2.1, file="graf2.1 Education-Sex.jpeg" )




gc()
# Table3 - % of Active Travel by Sex and Age groups -------------
# chart examples http://sape.inf.usi.ch/quick-reference/ggplot2/geom_errorbarh
# chart examples http://stackoverflow.com/questions/15513103/modifying-geom-ribbon-borders
# chart examples ggthemes http://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html
# http://stackoverflow.com/questions/12083387/how-to-create-line-chart-with-margins-of-error-in-r
# http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
#  http://www.noamross.net/blog/2013/11/20/formatting-plots-for-pubs.html

tab3pnad <- svyby(~v1410=="Yes" ,
                  ~agegroup + v0302,
                  design = sample.pnad08.18y ,
                  vartype="ci",
                  level = 0.95,
                  svyciprop)


tab3pns <- svyby(~v1410=="Yes" ,
                 ~agegroup + v0302,
                 design = sample.pns13.18y ,
                 vartype="ci",
                 level = 0.95,
                 svyciprop)


#add var year to table
tab3pnad$year <- 2008; tab3pns$year <- 2013

#Bind tables from each dataset into one single table 1
tab3 <- bind_rows(tab3pnad, tab3pns); colnames(tab3)[3] <- "Proportion"; remove(tab3pnad, tab3pns)
# Reorder values
# tab3$agegroup <- reorder(tab3$agegroup, tab3$Proportion)



graf3 <-  ggplot() + 
  geom_errorbarh(data=tab3, mapping=aes(y=agegroup, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(year)), height=0.2, size=1) + 
  geom_point(data=tab3, mapping=aes(y=agegroup, x=Proportion, color=factor(year), fill=factor(year)), size=3, shape=21 ) +
  facet_grid(. ~ v0302) +
  baseplot



graf3.1 <- ggplot(tab3, aes(x=agegroup, y=Proportion, group = year, color= factor(year))) +
  geom_line() +
  geom_ribbon(data=tab3, aes(ymin=ci_l, ymax=ci_u, color=factor(year), fill = factor(year)), linetype = 2, alpha=0.1) +
  facet_grid(v0302 ~ .) +
  scale_y_continuous(limits = c(0, 0.7))+
  baseplot


ggsave(graf3, file="graf3 Age-Sex.jpeg" )
ggsave(graf3.1, file="graf3.1 Age-Sex.jpeg" )


gc()

# Table4 - % of Active Travel by Sex and Metropolitan Area -------------


svytable(~v1410+v0302+metro, design = sample.pnad08.18y)

tab4pnad <- svyby(~v1410=="Yes" , # usando argumento method="mean" em PNAD (sugestao Djalma)
                  ~metro+v0302,
                  design = sample.pnad08.18y, # ATENCAO POS STRAT
                  vartype="ci",
                  level = 0.95,
                  na.rm=TRUE,
                  method="mean",
                  svyciprop)


tab4pns <- svyby(~v1410=="Yes" ,
                 ~metro + v0302,
                 design = sample.pns13.18y,
                 vartype="ci",
                 level = 0.95,
                 svyciprop)


#add var year to table
tab4pnad$year <- 2008; tab4pns$year <- 2013

#Bind tables from each dataset into one single table 1
tab4 <- bind_rows(tab4pnad, tab4pns); colnames(tab4)[3] <- "Proportion"; remove(tab4pnad, tab4pns)
# Reorder values
tab4$metro <- reorder(tab4$metro, tab4$Proportion)



graf4 <- ggplot() + 
  geom_errorbarh(data=tab4, mapping=aes(y=metro, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(v0302)), height=0.4, width=0.1, size=0.5) + 
  geom_point(data=tab4, mapping=aes(y=metro, x=Proportion, color=factor(v0302), fill=factor(v0302)), size=3, shape=21) +
  facet_grid(. ~ year) +
  baseplot

graf4.1 <- ggplot() + 
  geom_errorbarh(data=tab4, mapping=aes(y=metro, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(year)), height=0.4, width=0.1, size=0.5) + 
  geom_point(data=tab4, mapping=aes(y=metro, x=Proportion, color=factor(year), fill=factor(year)), size=3, shape=21 ) +
  facet_grid(. ~ v0302) +
  baseplot

ggsave(graf4, file="graf4 Metro-Sex.jpeg" )
ggsave(graf4.1, file="graf4.1 Metro-Sex.jpeg" )




######################!!!!!! RESULTS 30min   RESULTS 30min  RESULTS 30min ###################### 
######################!!!!!! RESULTS 30min   RESULTS 30min  RESULTS 30min ###################### 
######################!!!!!! RESULTS 30min   RESULTS 30min  RESULTS 30min ###################### 


# Table5- % of Active Travel by Sex and Urban vs Rural 30min -------------

#Contingency Table
svytable(~actv_commutetime30+v0302+urban, design = sample.pnad08.18y)

tab5pnad <- svyby(~factor( actv_commutetime30==1 ) ,
                  ~urban + v0302,
                  design = sample.pnad08.18y ,
                  vartype="ci",
                  level = 0.95,
                  svyciprop)

tab5pns <- svyby(~factor( actv_commutetime30==1 ) ,
                 ~urban + v0302,
                 design = sample.pns13.18y ,
                 vartype="ci",
                 level = 0.95,
                 svyciprop)


#add var year to table
tab5pnad$year <- 2008; tab5pns$year <- 2013

#Bind tables from each dataset into one single table 1
tab5 <- bind_rows(tab5pnad, tab5pns); colnames(tab5)[3] <- "Proportion"; remove(tab5pnad, tab5pns)

graf5 <- ggplot(tab5, aes(x=v0302, y=Proportion, fill=urban)) + 
  facet_grid(. ~ year) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  baseplot

graf5.1 <- ggplot(tab5, aes(x=factor(year), y=Proportion, fill=urban)) + 
  facet_grid(. ~ v0302) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  baseplot

ggsave(graf5, file="graf5 Urb-Rural Sex.jpeg" )
ggsave(graf5.1, file="graf5.1 Urb-Rural Sex.jpeg" )








gc()
# Table6 - % of Active Travel by Sex and Education -------------
# chart examples http://sape.inf.usi.ch/quick-reference/ggplot2/geom_errorbarh

tab6pnad <- svyby(~factor( actv_commutetime30==1 ) ,
                  ~v4745 + v0302,
                  design = sample.pnad08.18y ,
                  vartype="ci",
                  level = 0.95,
                  svyciprop)


tab6pns <- svyby(~factor( actv_commutetime30==1 ) ,
                 ~v4745 + v0302,
                 design = sample.pns13.18y ,
                 vartype="ci",
                 level = 0.95,
                 svyciprop)


#add var year to table
tab6pnad$year <- 2008; tab6pns$year <- 2013

#Bind tables from each dataset into one single table
tab6 <- bind_rows(tab6pnad, tab6pns); colnames(tab6)[3] <- "Proportion"; remove(tab6pnad, tab6pns)
# Reorder values - https://kohske.wordpress.com/2010/12/29/faq-how-to-order-the-factor-variables-in-ggplot2/
tab6$v4745 <- reorder(tab6$v4745, tab6$Proportion)

graf6 <-  ggplot() + 
  geom_errorbarh(data=tab6, mapping=aes(y=v4745, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(year)), height=0.4, width=0.1, size=0.5) + 
  geom_point(data=tab6, mapping=aes(y=v4745, x=Proportion, color=factor(year)), size=3, shape=21 ) +
  facet_grid(. ~ v0302) +
  baseplot


graf6.1 <-  ggplot() + 
  geom_errorbarh(data=tab6, mapping=aes(y=v4745, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(v0302)), height=0.4, width=0.1, size=0.5) + 
  geom_point(data=tab6, mapping=aes(y=v4745, x=Proportion, color=factor(v0302)), size=3, shape=21 ) +
  facet_grid(. ~ year) +
  baseplot

ggsave(graf6, file="graf6 Education-Sex.jpeg" )
ggsave(graf6.1, file="graf6.1 Education-Sex.jpeg" )




gc()
# Table7 - % of Active Travel by Sex and Age groups -------------
# chart examples http://sape.inf.usi.ch/quick-reference/ggplot2/geom_errorbarh
# chart examples http://stackoverflow.com/questions/15513103/modifying-geom-ribbon-borders
# chart examples ggthemes http://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html
# http://stackoverflow.com/questions/12083387/how-to-create-line-chart-with-margins-of-error-in-r
# http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
#  http://www.noamross.net/blog/2013/11/20/formatting-plots-for-pubs.html

tab7pnad <- svyby(~factor( actv_commutetime30==1 ) ,
                  ~agegroup + v0302,
                  design = sample.pnad08.18y ,
                  vartype="ci",
                  level = 0.95,
                  na.rm=TRUE,
                  svyciprop)


tab7pns <- svyby(~factor( actv_commutetime30==1 ) ,
                 ~agegroup + v0302,
                 design = sample.pns13.18y ,
                 vartype="ci",
                 level = 0.95,
                 svyciprop)


#add var year to table
tab7pnad$year <- 2008; tab7pns$year <- 2013

#Bind tables from each dataset into one single table 1
tab7 <- bind_rows(tab7pnad, tab7pns); colnames(tab7)[3] <- "Proportion"; remove(tab7pnad, tab7pns)
# Reorder values
# tab7$agegroup <- reorder(tab7$agegroup, tab7$Proportion)


graf7 <-  ggplot() + 
  geom_errorbarh(data=tab7, mapping=aes(y=agegroup, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(year)), height=0.4, width=0.1, size=0.5) + 
  geom_point(data=tab7, mapping=aes(y=agegroup, x=Proportion, color=factor(year), fill=factor(year)), size=3, shape=21 ) +
  facet_grid(. ~ v0302) +
  baseplot



graf7.1 <- ggplot(tab7, aes(x=agegroup, y=Proportion, group = year, color= factor(year))) +
  geom_line() +
  geom_ribbon(data=tab7, aes(ymin=ci_l, ymax=ci_u, color=factor(year), fill = factor(year)), linetype = 2, alpha=0.1) +
  facet_grid(v0302 ~ .) +
  scale_y_continuous(limits = c(0, 0.6)) +
  baseplot


ggsave(graf7, file="graf7 Age-Sex.jpeg" )
ggsave(graf7.1, file="graf7.1 Age-Sex.jpeg" )


gc()

# Table8 - % of Active Travel by Sex and Metropolitan Area -------------
tab8pnad <- svyby(~actv_commutetime30==1 ,
                  ~metro + v0302,
                  design = sample.pnad08.18y,
                  vartype="ci",
                  level = 0.95,
                  svyciprop)

tab8pns <- svyby(~actv_commutetime30==1 ,
                 ~metro + v0302,
                 design = sample.pns13.18y,
                 vartype="ci",
                 level = 0.95,
                 svyciprop)


#add var year to table
tab8pnad$year <- 2008; tab8pns$year <- 2013

#Bind tables from each dataset into one single table 1
tab8 <- bind_rows(tab8pnad, tab8pns); colnames(tab8)[3] <- "Proportion"; remove(tab8pnad, tab8pns)
# Reorder values
tab8$metro <- reorder(tab8$metro, tab8$Proportion)



graf8 <- ggplot() + 
  geom_errorbarh(data=tab8, mapping=aes(y=metro, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(v0302)), height=0.4, width=0.1, size=0.5) + 
  geom_point(data=tab8, mapping=aes(y=metro, x=Proportion, color=factor(v0302), fill=factor(v0302)), size=3, shape=21 ) +
  facet_grid(. ~ year) 

graf8.1 <- ggplot() + 
  geom_errorbarh(data=tab8, mapping=aes(y=metro, x=Proportion, xmin=ci_u, xmax=ci_l, color=factor(year)), height=0.4, width=0.1, size=0.5) + 
  geom_point(data=tab8, mapping=aes(y=metro, x=Proportion, color=factor(year), fill=factor(year)), size=3, shape=21 ) +
  facet_grid(. ~ v0302) +
  baseplot

ggsave(graf8, file="graf8 Metro-Sex.jpeg" )
ggsave(graf8.1, file="graf8.1 Metro-Sex.jpeg" )




# Table20 - Motorization by Sex and Metropolitan Area -------------
#Contingency Table
svytable(~V2032+urban, design = sample.pnad08dom)
svytable(~V2032+urban, design = sample.pns13dom)

# PNAD table 20
# Get General Proportions
table <- svyby(~v2032,~urban, sample.pnad08dom, svymean, vartype="ci", level = 0.95, na.rm=T)
# Reshape data frame
table <- gather(table,"urban",n,2:13)
table[,2] <- gsub("v2032", "", table[,2]) #rename collums
# Slice table to re-organize it
obs <- table[1:8,]
ci_l <- table[9:16,3]; ci_l <- as.data.frame(ci_l)
ci_u <- table[17:24,3]; ci_u <- as.data.frame(ci_u)
# Get table binding all the slices
tab20pnad <- bind_cols(obs, ci_l,ci_u)

# PNS table 20
# Get General Proportions
table <- svyby(~v2032,~urban, sample.pns13dom, svymean, vartype="ci", level = 0.95, na.rm=T)
# Reshape data frame
table <- gather(table,"urban",n,2:13)
table[,2] <- gsub("v2032", "", table[,2]) #rename collums
# Slice table to re-organize it
obs <- table[1:8,]
ci_l <- table[9:16,3]; ci_l <- as.data.frame(ci_l)
ci_u <- table[17:24,3]; ci_u <- as.data.frame(ci_u)
# Get table binding all the slices
tab20pns <- bind_cols(obs, ci_l,ci_u)

#add var year to table
tab20pnad$year <- 2008; tab20pns$year <- 2013

#Bind tables from each dataset into one single table 1
tab20 <- bind_rows(tab20pnad, tab20pns); colnames(tab20)[3] <- "Proportion"; remove(tab20pnad, tab20pns, ci_l, ci_u,obs,table)
# Reorder values
tab8$metro <- reorder(tab8$metro, tab8$Proportion)

# Create chart 
graf20 <-  ggplot(tab20, aes(x=factor(year), y=Proportion, fill=variable)) + 
  facet_grid(. ~ urban) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  baseplot
# save chart       
ggsave(graf20, file="graf20 Motorization urban-rural.jpeg" )










# Nice Chart / Plotingexamples-------
! - http://minimaxir.com/2015/02/ggplot-tutorial/
  http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions
  theme_minimal economist -http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
http://psybercity.co.uk/2013/12/26/function-to-overlay-histograms/
  # GRID de graficos  http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
  http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
  
  