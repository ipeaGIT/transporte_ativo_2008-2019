# Create survey design for PNS 2019 data 
# 1) Load packages  ------
rm(list=ls())
gc(reset=TRUE)
library(PNADcIBGE) # devtools::install_github("Gabriel-Assuncao/PNADcIBGE")
library(survey)
library(srvyr)
library(data.table)
library(microdadosBrasil) # devtools::install_github("lucasmation/microdadosBrasil")

# 2) Read files -----

pns2019 <- readr::read_rds("../../data/transporte_ativo_2008-2019/pns2019.Rds")


# M001    | Entrevista do adulto selecionado
#         | 1	Realizada
#         | 2	Recusa
#         | 3	Morador não encontrado
#         | 9	Ignorado
#         | Não aplicável
# UPA_PNS | UPA
# V0024   | Strata
# V00293  | Dominio de pos-estrato 2
# V0029	  | Peso do morador selecionado sem calibracao
# V00291  | Peso do morador selecionado com calibracao
# V00292  | Projecao da populacao para moradores selecionados
# V00283  | Domínio de projeção para domicílio e moradores

# There should be no Missings (NA) in Design Variables
# Count  missing values (NAs)

anyNA(pns2019$V00291)                # TRUE
sum(is.na(pns2019$V00291))           # 202880
sum(is.na(pns2019$V0029))            # 202880
length(which(pns2019$M001 == 1))     # 90846

# Subset PNS with individuals who answered the detailed questionnaire only
# This eliminates observations with missing values in the weight variable

pns2019Det <- data.table::copy(pns2019)[!is.na(V0029) & M001 == 1]



#define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
# set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    


# Cria objeto de desenho da amostra                      

sample_pns <- survey::svydesign(data = pns2019Det,
                                id = ~UPA_PNS,    # PSU
                                strata = ~V0024,  # Strat
                                weights = ~V0029, # PesoPessoa: usar peso original
                                nest = TRUE)

## Agora é preciso pós-estratificar:
## A definição dos pós-estratos e os totais populacionais usados são dados por:

## post-estratification of sample design
# V00292  | Projecao da populacao para moradores selecionados
# V00293  | Dominio de pos-estrato 2

post_pop <- unique( pns2019Det[,c("V00293","V00292")] )
names(post_pop) <- c("V00293","Freq")

sample_pns_pos <- survey::postStratify(sample_pns, ~V00293, post_pop)


# 3) Save survey ------
readr::write_rds(x = sample_pns_pos
        ,file = "../../data/transporte_ativo_2008-2019/sample_pns2019_pos.rds"
        , compress = "gz")
