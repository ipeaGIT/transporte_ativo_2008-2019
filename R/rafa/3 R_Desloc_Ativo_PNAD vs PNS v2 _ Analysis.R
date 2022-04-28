tabelas com varias var
http://stackoverflow.com/questions/23396528/multiple-factors-in-svyby-vs-svytotal

Graficos
http://www.r-bloggers.com/visualizing-survey-data-comparison-between-observations/
  
  
# Analyze Active Travel in Brazil using data from PNS 2013 and PNAD2008 (suplemento de Saude)
# Oxford 16/03/2015

# R version 3.1.2 (64 bits)
# by Rafael Pereira - urbandemographics.blogspot.com


# Set working directory
setwd("C:/Users/rafa/Desktop/ActiveTravel_Pnad-PNS")


############# Necessary Packages -----------



    



############ Load DATA Sets ############
# system.time(pnad2008 <- fread("pnad2008.csv"))
#system.time(pns2013 <- fread("pns2013.csv"))
pns2013 <- readRDS("pns2013.Rds")
pnad2008 <- readRDS("pnad2008.Rds")

pns2013dom <- readRDS("pns2013dom.Rds")
pnad2008dom <- readRDS("pnad2008dom.Rds")

# system.time(pns2013dom <- fread("PNS2013dom.csv"))
# system.time(pnad2008dom <- fread("pnad2008dom.csv"))

 

  # Keep only important variables
    #pnad2008 <- select(pnad2008, v0101,uf,v0102,v0103,v0403,v4729,v4609, v4732,v8005,v0302,v0404,v4803,v4838,v4728,v4727,v0602,v9005,v4805,v4706,v4810,v4720,v4721,v4721,v4745,v9054,v9055,v9056,v9057,v1409,v14091,v1410,v1411, v4602, v4618, v4617, upa, region, AGE,agegroup, urban, metro, DecileBR,Vcount, v1410mod,v1411mod,v9057mod,actv_commutetime30,pre_wgt)
    pns2013 <- select(pns2013, 
                V0031,   # pnad compat_ metropolitan area
                V0026,   # pnad compat_ urban
                v4745,   # pnad compat_ Educational attainment
                v0302,   # pnad compat_ Sex
                v4727,   # pnad compat_ Metropolitan area
                v1410,   # pnad compat_ Active Travel
                v2032,  # pnad compat_ Car or motorcycle ownership
                V0001,   # state
                C006,    # sex
                C009,    # race
                C008,    # age
                VDD004,  # Educational attainment
                P040,    # Active commute
                P04101,  # Active commute time (hours)
                P04102,  # Active commute time (minutes)
                P04301, # active travel to habitual activities
                P04302, # active travel time to habitual activities
                P00101, # Weight
                P00401, # Height
                N001, # health perception
                O009, # car accident
                O011, # travel mode when injured
                O014, # accident hindered habitual activities
                O020, # any sequel and / or disability due to this traffic accident
                Q002, # Ever diagnosed with hypertension 
                Q003, # age at diagnosis for hypertension
                Q030, # Ever diagnosed with diabetes 
                Q031, # age at diagnosis for diabetes
                Q060, # Ever diagnosed with high cholesterol 
                Q061,  # age at diagnosis for high cholesterol
                
                V0025,   # person selected for long questionaire
                M001,    # Type of interview
                UPA_PNS, # UPA
                V0024,   # Strata
                V0029,   # person sample weight without calibratio
                V00291,  # person sample weight with calibration
                V00292,  # Population projection
                V00283,  # Domínio de pós-estrato 1
                V00293,  # Domínio de pós-estrato 2
                A01817,  # Motorcycle ownership
                A020,    # Car ownership (number of cars)
                v2032,   # Vehicle in the household, compatible with PNAD
                dummyVehicle,
                v1410, # dummy active travel , compatible with PNAD
                year, region,
                AGE, agegroup, edugroup, urban, metro, vcount, 
                actv_commutetime30, actv_commutetime,actv_traveltimehabacts,total_actvtraveltime, physicallyactive30, # commute variables
                totalhouseholdincome, v4721, decileBR, quintileBR, quintileRegion, quartileRegion, quintileMetro, quartileMetro) # income variables
                
    

                

############## PNS Create survey design  -----------

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
                            nest = TRUE
                            )
  
  ## Agora é preciso pós-estratificar:
  ## A definição dos pós-estratos e os totais populacionais usados são dados por:
  
  ## post-estratification of sample design
  post_pop <- unique( subset(pns2013Det, select= c(V00293,V00292) ))
  names(post_pop)<-c("V00293","Freq")
  
  sample.pns13.pos <- postStratify(sample.pns13, ~V00293, post_pop)
  

  #Subset design population above 18 years old
  sample.pns13.18y <- subset(sample.pns13.pos, C008>17)
  

    
  
  
  
############## PNAD Create survey design  -----------

  #define como imputar variancia quando houver apenas um domicilio (PSU) no estrato
   options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu 
  
  #Cria objeto de desenho da amostra                      
   sample.pnad08 <- svydesign(
                              data = pnad2008,
                              id = ~v4618, #PSU
                              strata = ~v4617, #Strat
                              weights = ~pre_wgt, #person weight v4729
                              nest = TRUE
                              )

   # postStratify pnad2008 (Djalma suggestion)
   post.pop <- unique(data.frame(v4609=as.character(pnad2008$v4609), Freq= as.numeric(pnad2008$v4609)))
   sample.pos <- postStratify(design=sample.pnad08, strata=~v4609, population=post.pop)
   
  # Subset Survey design of people above 18 years old in order to make PNAD 
  # compatible with PNS, in which only people above 18 answer the detailed quest
  sample.pnad08.18y <- subset(sample.pos, v8005>=17)
  

  

  
    
##### Household Design  -----------

  # PNAD households Survey design
  
    # This eliminates observations with missing values in the weight variable
      summary(pnad2008dom$v4611)
      pnad2008dom <- pnad2008dom[!(is.na(pnad2008dom$v4611))]
  
  options( survey.lonely.psu = "adjust" )
  sample.pnad08dom <- svydesign(data = pnad2008dom,
                                id = ~v4618, #PSU
                                strata = ~v4617, #Strat
                                weights = ~v4611, #household weight
                                nest = TRUE)


  # PNS households Survey design
  length(which(is.na(pns2013dom$V00281)))   # Count  missing values (NAs), There should be no Missings (NA) in Design Variables
  pns2013dom <- pns2013dom[!(is.na(pns2013dom$V00281))]   # This eliminates observations with missing values in the weight variable
  sample.pns13dom <- svydesign(data = pns2013dom,
                               id = ~UPA_PNS, #PSU
                               strata = ~V0024, #Strat
                               weights=~V00281, #Peso household
                               nest = TRUE  )


  
  
# clean objects and memory
  rm(list=setdiff(ls(), c("sample.pns13dom", "sample.pnad08dom", "sample.pns13.18y", "sample.pnad08.18y", "pnad2008", "pns2013") ))
  gc(reset = T)

  
  
  
  
  
  
######################!!!!!! RESULTS   RESULTS  RESULTS ###################### 
######################!!!!!! RESULTS   RESULTS  RESULTS ###################### 
######################!!!!!! RESULTS   RESULTS  RESULTS ###################### 


# Create Basic Aesthetic stantard for the plots

  # delete
  # baseplot <- theme(
  #   panel.background = element_rect(colour = NULL, fill = "gray98"),
  #   panel.grid.major = element_line(colour = "gray80"),
  #   panel.grid.minor = element_blank(),
  #   axis.text.y  = element_text(face="bold"),
  #   axis.text.x  = element_text(face="bold"),
  #   axis.ticks = element_blank(),
  #   strip.text.x = element_text(size = 11, face ="bold"),
  #   strip.background = element_rect(colour = "white", fill = "white", size = 4), #muda estilo de facet_grid boxes
  #   legend.key = element_rect(fill = "white"),
  #   legend.title=element_blank(),
  #   legend.text = element_text(size = 11)
  #   )
  
  baseplot <- theme_minimal() +
    theme( 
      axis.text  = element_text(face="bold"),
      strip.text = element_text(size = 11, face ="bold"),
      legend.text = element_text(size = 11)
      )
  



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
  
