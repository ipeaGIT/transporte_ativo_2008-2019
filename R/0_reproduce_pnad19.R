rm(list=ls())
library(PNSIBGE)
library(data.table)
library(magrittr)

# Read -----
# PNS 2019 
sas_path_2019 <- "../../data-raw/PNS/2019/input_PNS_2019.sas"
txt_path_2019 <- "../../data-raw/PNS/2019/pns_2019_20220525/PNS_2019.txt"
pns2019_ibge <- PNSIBGE::read_pns(microdata = txt_path_2019
                                  ,input_txt = sas_path_2019
                                  ,vars = NULL)
pns2019 <- data.table::copy(pns2019_ibge)
data.table::setDT(pns2019)

# PNS 2013
sas_path_2013 <- "../../data-raw/PNS/2013/input_PNS_2013.sas"
txt_path_2013 <- "../../data-raw/PNS/2013/pns_2013_20200825/PNS_2013.txt"
pns2013_ibge <- PNSIBGE::read_pns(microdata = txt_path_2013
                                  ,input_txt = sas_path_2013
                                  ,vars = NULL)
pns2013 <- data.table::copy(pns2013_ibge)
data.table::setDT(pns2013)

# Check motivos viagem ----
# | P040     | Para ir ou voltar do trabalho, o(a) Sr(a) faz algum       |  both   |
# |          | trajeto a pé ou de bicicleta?                             |         |
# |          | 1  Sim, todo o trajeto ;                                  |         |
# |          | 2  Sim, parte do trajeto;                                 |         |
# |          | 3  Não; Não aplicável                                     |         |
# | P04001   | Quantos dias por semana o(a) Sr(a) faz algum trajeto a pé |   2019  |
# |          | ou bicicleta?                                             |         |
# | P04101   | numero de horas da ativ acima (ida e volta)               |   both  |
# | P04102   | numero de minut da ativ acima (ida e volta)               |   both  |
# | P042     | Nas suas atividades habituais (tais como ir, ou levar     |         |
# |          | alguém, a algum curso, escola ou clube), quantos dias por |         |
# |          | semana o(a) Sr(a) faz alguma atividade que envolva        |         |
# |          | deslocamento a pé ou bicicleta?                           |         |
# |          | (0 = Nunca ou menos de uma vez por semana)                |         |
# | P04301   | numero de horas da ativ acima (ida e volta)               |   both  |
# | P04302   | numero de minut da ativ acima (ida e volta)               |   both  |
# |   C006   | Sexo                                                      |   both  |
# |          | 1  Masculino                                              |         |
# |          | 2  Feminino                                               |         |
# pre filter
pns2019cp <- data.table::copy(pns2019) %>% 
  .[P040 == "1" | P040 == "2" | P04001 > 0,]

table(pns2019cp$P042,useNA = "always")
table(pns2019cp$P04001,useNA = "always")

# adiciona legenda
dic_sexo <- data.frame(C006 = as.character(1:2)
                       , C006_name = c("Masculino","Feminino")) 
dic_raca <- data.frame(C009 = as.character(c(1:5,9)),
                       C009_name = c("Branca","Preta","Amarela"
                                     ,"Parda","Indígena","Ignorado"))

pns2019cp <- pns2019cp[dic_sexo, on = "C006"]
pns2019cp <- pns2019cp[dic_raca, on = "C009"]


# razao_dias = dias atividades habituais / dias total
# razao_dias = P042 / P04001

nrow( pns2019cp )                 # 18717
nrow( pns2019cp[P042 > P04001,] ) # 1940 (~10 %)

pns2019cp[, razao_dias := P042 / (P04001 + P042)]
pns2019cp[,mean(razao_dias,na.rm = TRUE), by = C006_name]
pns2019cp[,mean(razao_dias,na.rm = TRUE), by = .(C009_name,C006_name)]

# media do numero de dias (atividades habituais)
pns2019cp[,summary(P042,na.rm = TRUE), by = C006_name]

# media do numero de dias (atividades totais)
pns2019cp[,mean(P04001,na.rm = TRUE), by = C006_name]

# proporcao de dias por genero
prop_dias_P042 <- pns2019cp[,.N,by = .(C006_name,P042)]
prop_dias_P042[,prop := round(100 * N/sum(N), 2),by = .(C006_name)]
prop_dias_P042[,dias_f := factor(x = P042,levels = 0:7)]
prop_dias_P042[,name := "ativ_habituais"]
prop_dias_P042[,P042 := NULL]

prop_dias_P04001 <- pns2019cp[,.N,by = .(C006_name,P04001)]
prop_dias_P04001[,prop := round(100 * N/sum(N), 2),by = .(C006_name)]
prop_dias_P04001[,dias_f := factor(x = P04001,levels = 0:7)]
prop_dias_P04001[,name := "trabalho"]
prop_dias_P04001[,P04001 := NULL]

prop_dias <- rbind(prop_dias_P04001,prop_dias_P042)
prop_dias[,name_f := factor(name,c("total","ativ_habituais"))]

ggplot(prop_dias)+
  geom_bar(aes(x = dias_f, y = prop, fill = factor(C006_name))
           ,stat = "identity",position = "dodge2")+
  facet_wrap(~name_f,ncol = 1,scales = "free_y")+
  labs(x = "numero de dias da semana")

pns2019cp$P04001 %>% summary()
pns2019[
  as.numeric(V0026) == 1 #&   # urbano
  P040 == "1"                # 1  Sim, todo o trajeto
  ,.N
  ,by = .(VDD004A)]            # Nivel de instrucao



# Check escolaridade ----
# Dicionario
# |   C008   | Idade do morador na data de referência                   |
# |   V002   | Tipo de situação censitária                              |
# |          | 1  Urbana                                                |
# |          | 2  Rural                                                 |
# |   P040   | Para ir ou voltar do trabalho, o(a) Sr(a) faz algum      |
# |          | trajeto a pé ou de bicicleta?                            |
# |          | 1  Sim, todo o trajeto ;                                 |
# |          | 2  Sim, parte do trajeto;                                |
# |          | 3  Não; Não aplicável                                    |
# | VDD004A  | Nivel de instrucao mais alto alcançado                   |
# |          | 1  Sem instrução                                         |
# |          | 2  Fundamental incompleto ou equivalente                 |
# |          | 3  Fundamental completo ou equivalente                   |
# |          | 4  Médio incompleto ou equivalente                       |
# |          | 5  Médio completo ou equivalente                         |
# |          | 6  Superior incompleto ou equivalente                    |
# |          | 7  Superior completo                                     |
# |   C006   | Sexo                                                     |
# |          | 1  Masculino                                             |
# |          | 2  Feminino                                              |
library("PNSIBGE")

pns2013 <- PNSIBGE::get_pns(year=2013, design=TRUE)
readr::write_rds(pns2013,"../../data-raw/PNS/2013/pns2013_design.rds",compress = "gz")

pns2013 <- PNSIBGE::get_pns(year=2013, design= FALSE)
readr::write_rds(pns2013,"../../data-raw/PNS/2013/pns2013.rds",compress = "gz")




pns2013_recorte <- subset(pns2013, C008>=18 & V0026=="Urbano" & P040=="Sim, todo o trajeto")
pns2019_recorte <- subset(pns2019, C008>=18 & V0026=="Urbano" & P040=="Sim, todo o trajeto")

cbind(table(pns2013$VDD004A),prop.table(table(pns2013$VDD004A)))

cbind(table(pns2019$VDD004A),prop.table(table(pns2019$VDD004A)))

cbind(table(pns2013_recorte$VDD004A),prop.table(table(pns2013_recorte$VDD004A)))

cbind(table(pns2019_recorte$VDD004A),prop.table(table(pns2019_recorte$VDD004A)))
