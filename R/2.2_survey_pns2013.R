

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