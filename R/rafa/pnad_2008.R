#Analise descritiva de Deslocamento Ativo na PNAD2008 (suplemento de Saude), Oxford 23/03/2014#

# R versAo 2.15-12 (64 bits)

#Define pasta de referencia
setwd("C:/Users/rafa/Desktop/Dropbox/Desloc Ativo1/R code Deslocamento_Ativo")




#############!!!!!! Instala pacotes -----------

    # 'Survey' para amostras complexas no R
    install.packages("survey")
    
    # 'taRifx' - manipula bases entre outras funcoes
    install.packages("taRifx")
    
    # 'downloader' - para baixar dados
    install.packages("downloader")
    
    # 'plyr' - para manusear vbases (conta numero de missing values)
    install.packages("plyr")
    
    # 'foreign' - para importar dados de STATA e SPSS
    install.packages("foreign")
    
    #aponta local de instalacao do Java
      Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre7")   
      
    # 'xlsx' - para ler e ciar arquivos em Excel
    install.packages("xlsx") 
    install.packages("rJava") 
    
    # 'descr' - para fazer tabela de freq com expansao da amostra
    install.packages("descr")
    
    # 'IBGEPesq' para ler dados do IBGE // Se necess?rio, baixe pacote 'IBGEPesq' no site do IBGE "http://www.ibge.gov.br/home/estatistica/populacao/trabalhoerendimento/pnad2009/microdados.shtm"
    install.packages("IBGEPesq")
    
    # 'dicionariosIBGE' com dicionarios de leitura de microdados do IBGE
    install.packages("dicionariosIBGE")
    
    # 'foreach' para fazer Loops
    install.packages("foreach")

    # 'fanplot' para fazer graficos de linha+intervalos
    install.packages("fanplot")

    # 'ggplot2' ............................
    install.packages("ggplot2")

    # 'reshape' ............................
    install.packages("reshape")

    # 'plyr and dplyr' para fazer split-apply-combine de datasets
    install.packages("plyr")
    install.packages("dplyr")             
                     
                     

    
    require(survey)
    require(taRifx)
    require(downloader)
    require(plyr)                       
    require(foreign)
    require(rJava)
    require(xlsx)
    require(descr)  
    library(IBGEPesq)
    library(dicionariosIBGE)
    library(foreach)
    library(fanplot)
    require(ggplot2)
    require(reshape)
    require(plyr)

    

    options(digits=4)




############!!!!!! Prepara Base de dados pnad2008 e RMS ############

############ Importacao de microdados da PNAD reponderada do site do IBGE   #####################   

  #Baixe do site do IBGE os dados da pnad2008 (bases de pessoas e domicilios) em formato .rar e salve no working directory

      download.file("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/reponderacao_2001_2009/PNAD_reponderado_2008.zip",
                    "PNAD2008.rar", quiet = FALSE)

  # Unzipa arquivo 

      unzip("PNAD2008.rar")


  # Leitura dos arquivos .TxT de microdados da PNAD reponderada

      #carrega dicionario da pnad2008
      data(dicPNAD2008)

      #Leitura de pes2008  
        # Se der erro 'You must provide the number of lines of the data file dicpes2008', basta recarregar o pacote IBGEpesq

                pes2008r <- le.pesquisa(dicionario=dicpes2008,
                       pathname.in="./2008/dados/PES2008.txt", 
                       codigos=c("V0101","UF","V0102","V0103","V0403","V4729","V4732","V8005",
                                 "V0302","V0404","V4803","V4838","V4728","V4727","V0602","V9005","V4805","V4706","V4810",
                                 "V4720","V4721","V4742","V9054","V9055","V9056","V9057","V1409","V14091","V1410","V1411"), 
                       tbloco = 100000,
                       nlines = 1)

                                              
                       # v0101  Ano de referencia
                       # uf     Unidade da Federacao
                       # v0102  Numero de controle
                       # v0103  Numero de serie
                       # v0403  Numero da familia
                       
                       # v4729  Peso da pessoa
                       # v4732  Peso da familia
                       
                       # v8005  Idade do morador na data de referencia
                       # v0302  Sexo
                       # v0404  Cor ou raca
                       # v4803  Anos de estudo
                       # V4838  Grupo de anos de estudo
                       
                       # v4728  Urbano.1:3 e Rural.4:8
                       # v4727  Area Metropolitana.1 nao-metro.2:3
                       
                       # v0602  Frequenta Creche ou escola 2.sim 4.nao
                       # v9005  Numero de trabalhos que tinha na semana de referencia
                       # v4805  Ocupado.1 x Desocupado.2
                       # v4706  Posicao na ocupacao
                       # v4810  Grupamentos ocupacionais do trabalho
                       
                       # v4810 Rendimento mensal do trabalho principal $
                       # v4720 Rendimento mensal de todas as fontes $
                       # v4721 Rendimento mensal domiciliar $
                       # v4742 Rendimento mensal domiciliar per capita

                       # V1409   Dificuldade para andar - 1  N?o  consegue                                             
                       # V14091  Dificuldadepara fazer compras - 1  Nao  consegue 
                       # V9054  Tipo de estabelecimento onde trabalha
                       # V9055  Mora no mesmo terreno do local de trabalho
                       # V9056  Ia direto do domic�???lio em que morava para o trabalho
                       # v9057  Tempo casa-trabalho
                       # v1410  Costuma ir a pé ou bicicleta para o trabalho - Sim.2 Não.4
                       # v1411  Tempo gasto para ir e voltar do trabalho
                       

        # Salva a base de pessoas
              save(list="pes2008", file="pes2008.RData")

                     
      #Leitura de dom2008
             
              dom2008r <- le.pesquisa(dicionario=dicdom2008,
                       pathname.in="./2008/dados/DOM2008.txt", 
                       codigos=c("V0101","UF","V0102","V0103","V4107","V4605","V4607","V4602", "V4609", "V4611","UPA","V4617", "V4618","V4619"), 
                       tbloco = 100000,
                       nlines = 100)
                                            
                       
                       # v0101  Ano de referencia
                       # UF     Unidade da Federacao
                       # v0102  Numero de controle
                       # v0103  Numero de serie
                       # v4107  Codigo de area censitaria
                       # v4605  Probabilidade do municipio
                       # v4607  Probabilidade do setor
                       
                       # v4602    Estrato
                       # v4609    Projecao de populacao
                       # v4611  	Peso do domicilio
                       # UPA  	  Delimitacao do munic?pio
                       # v4617    STRAT - Identificacao de estrato de munic?pio auto-representativo e n?o auto-representativo
                       # v4618  	PSU - Unidade primaria de amostragem
                       # v4619  	Fator de subamostragem
 

        # Salva a base de pessoas
                save(list="dom2008", file="dom2008.RData")

              load("pes2008.RData")
              load("dom2008.RData")

  #Merge  dos  arquivos  de  domicilio  e  pessoas---------------------------------
          
          # Ordena (sort) por VariaVeis chaVe para concatenar com base de pessoas
            pes2008 <- sort (pes2008,formula=~+UF+V0102+V0103)
            dom2008 <- sort (dom2008,formula=~+UF+V0102+V0103)
          
          #Comando de Merge
                       
            pnad2008 <- merge(dom2008, pes2008, by=c("UF", "V0102", "V0103")) # 391.868 obs e 34 VariaVeis
                                           

                  #Checagem da variavel 'Estrato'V4602. Se nao tiver missing, sinal de que a juncao das bases de pes e dom deu certo
                       attach(pnad2008)
                       count <- list(V4602)
                       summary(count) #medidas de posicao
                       class(V4602) #Verifica classe da VariaVel ESTRATO (deVe ser eh numerica/integer)
                      

                      # Verifica se tem algum missing
                       summary(V4602)
                       mean(V4602,na.rm=FALSE) #tira a media do Vetor, mas retorna NA se houVer algum missing
                       mean(V4602,na.rm=TRUE)
                       
                       
                       #outra maneira /  Informa as linhas que tem Valor  missing NA
                       #identify_NA_rows <- which(pnad2008$V4602==NA)
                       identify_NA_rows <- which(is.na(pnad2008$V4602))
                       print(identify_NA_rows)
                       
                       any(is.na(pnad2008$V4602))
        
            #limpa objetos do workspace
                        remove(dom2008, pes2008, count, identify_NA_rows, dicdom2008, dicpes2008)




  # Adiciona novas VariaVeis para Base pnad2008 ---------------------------------
              
    attach(pnad2008)
                  

                  # Transforma algumas Variaveis de fator para numerica 
                      pnad2008$V8005 <- as.numeric(as.character(pnad2008$V8005)) #idade
                      pnad2008$V4742 <- as.numeric(as.character(pnad2008$V4742)) #renda dom per capita
                      attach(pnad2008)

                  #cria VariaVel com o numero da linha
                       rownames(pnad2008) = NULL
                       pnad2008$row.number <- 1:nrow(pnad2008)
                       
                       #cria VariaVel com Valor 1 para cada linha     
                       pnad2008$Vcount <- 1
                 
                  
                       
                  # cria VariaVel de Regiao
                       
                       pnad2008$regiao[UF < 20] <- "Norte"
                       pnad2008$regiao[UF>20 & UF<30] <- "Nordeste"
                       pnad2008$regiao[UF>30 & UF<40] <- "Sudeste"
                       pnad2008$regiao[UF>40 & UF<50] <- "Sul"
                       pnad2008$regiao[UF>50 & UF<60] <- "Centro Oeste"
                 class(pnad2008$regiao)    


                 # Recode Urban vs Rural Variable
                 pnad2008$urban[pnad2008$v4105<4] <-"Urban"
                 pnad2008$urban[pnad2008$v4105>3 & pnad2008$v4105<9] <-"Rural"
                 
                 # Recode metropolitan area variable
                 pnad200814$metro[pnad200814$V4727==1] <-"metro"
                 pnad200814$metro[pnad200814$V4727!=1] <-"not metro"
                 
                 
                 
                  # cria VariaVel Grupos de idade agegroup
                                               
                        pnad2008$AGE[V8005>=0 & V8005<14] <-"0-13"
                        pnad2008$AGE[V8005>13 & V8005<25] <-"14-24"
                        pnad2008$AGE[V8005>24 & V8005<35] <-"25-34"
                        pnad2008$AGE[V8005>34 & V8005<45] <-"35-44"
                        pnad2008$AGE[V8005>44 & V8005<55] <-"45-54"
                        pnad2008$AGE[V8005>54 & V8005<65] <-"55-64"
                        pnad2008$AGE[V8005>64] <-"65+"

                      #pnad2008$ageTEST<- cut(pnad2008$V8005, 
                      #                        breaks = c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,65,70, Inf), 
                      #                        labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70+"), 
                      #                        right = F)

                       pnad2008$agegroup[V8005<5] <-"0-4"
                       pnad2008$agegroup[V8005>4 & V8005<14] <-"5-13"
                       pnad2008$agegroup[V8005>13 & V8005<20] <-"14-19"
                       pnad2008$agegroup[V8005>19 & V8005<25] <-"20-24"
                       pnad2008$agegroup[V8005>24 & V8005<30] <-"25-29"
                       pnad2008$agegroup[V8005>29 & V8005<35] <-"30-34"
                       pnad2008$agegroup[V8005>34 & V8005<40] <-"35-39"
                       pnad2008$agegroup[V8005>39 & V8005<45] <-"40-44"
                       pnad2008$agegroup[V8005>44 & V8005<50] <-"45-49"
                       pnad2008$agegroup[V8005>49 & V8005<55] <-"50-54"
                       pnad2008$agegroup[V8005>54 & V8005<60] <-"55-59"
                       pnad2008$agegroup[V8005>59 & V8005<65] <-"60-64"
                       pnad2008$agegroup[V8005>64 ] <-"65>"
        
           
   




                  # limpar Missing Values da Var 4742 para construir decil e quintil de renda
                      #Antes da limpeza==391.868 obs. A lipeza tirou 12.652 cases (3,2%)
                       summary(V4742)
                       pnad2008 <- pnad2008[pnad2008$V4742<999999999999, ] #elimina obserVacoes missing na Var.  de renda Dom per capita.
                       pnad2008 <- subset(pnad2008, V4742>= 0) #elimina obserVacoes NA
                       attach(pnad2008)
                       summary(V4742)
                       

                  # Cria Var. Decil de Renda domiciliar per capita (V4742)
                       pnad2008$DecileBR <- cut(pnad2008$V4742, 
                                              breaks= quantile(pnad2008$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                              include.lowest= TRUE, labels= c(1:10))
                       
                      # Tabelas de conferencia
                          table(pnad2008$DecileBR) #Numero de casos dentro de cada Decil tem que ser igual/proximo
                          attach(pnad2008)
                          by(pnad2008[c("V4742")], DecileBR, summary) # Decricao geral distriuicao de renda dentro de cada Decil

pctime <- proc.time()

      #salva a base até aqui
              save(list="pnad2008", file="pnad2008.RData")
              load("pnad2008.RData")
              attach(pnad2008)




                      # Para pessoas ocupadas e q nao responderam a V1410, imputa q desloc ativo=0
                      # Porque?: Se faz isso apenas para elas contarem no denominador na hora de calcular a proporcao de desloca ativo
                      # inclui na var V1410 1.393 casos 

                            pnad2008$V1410 <- ifelse((pnad2008$V4805==1 & is.na(pnad2008$V1410)), 
                                                0, pnad2008$V1410 <- pnad2008$V1410) 


                      # Para pessoas ocupadas e q nao responderam a V1411, imputa tempo de desloc=0
                      # Porque?: Ibdem.
                      # inclui na var V1410 120.436 casos
                      
                             pnad2008$V1411 <- ifelse((pnad2008$V4805==1 & is.na(pnad2008$V1411)), 
                                                 0, pnad2008$V1411 <- pnad2008$V1411) 
                      
                      
  



####!!!!!! Cria Base de dados de RMS-------------------

    #Cria base soh com moradores de RMS
        RMS <- subset(pnad2008, V4727==1)

    # cria subset de cada UF
        UFS <-  c(15,23,26,29,31,33,35,41,43,53) #Lista das UFS analizadas
        for (i in UFS) { assign(paste("RMS.",i,sep=""), subset(RMS, UF==i))}

                         
    # Cria var. DecileRM de acordo com a distribuicao de renda de cada RM

# Tentar otimizar esse trecho do codigo usando dplyr ou plyr http://plyr.had.co.nz/

          RMS.15$DecileRM <- cut(RMS.15$V4742, breaks=quantile(RMS.15$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.23$DecileRM <- cut(RMS.23$V4742, breaks=quantile(RMS.23$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.26$DecileRM <- cut(RMS.26$V4742, breaks=quantile(RMS.26$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.29$DecileRM <- cut(RMS.29$V4742, breaks=quantile(RMS.29$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.31$DecileRM <- cut(RMS.31$V4742, breaks=quantile(RMS.31$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.33$DecileRM <- cut(RMS.33$V4742, breaks=quantile(RMS.33$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.35$DecileRM <- cut(RMS.35$V4742, breaks=quantile(RMS.35$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.41$DecileRM <- cut(RMS.41$V4742, breaks=quantile(RMS.41$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.43$DecileRM <- cut(RMS.43$V4742, breaks=quantile(RMS.43$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))
          
          RMS.53$DecileRM <- cut(RMS.53$V4742, breaks=quantile(RMS.53$V4742, 
                                                               probs= seq(0, 1, by= 0.1)),
                                 include.lowest= TRUE, labels= c(1:10))


    # Junta novamente todas as bases em RMS e limpa workspace (RMS com 138.114 obs. of 44 variables)
          RMS <- rbind(RMS.15,RMS.23,RMS.26,RMS.29,RMS.31,RMS.33,RMS.35,RMS.41,RMS.43,RMS.53)
          remove (RMS.15,RMS.23,RMS.26,RMS.29,RMS.31,RMS.33,RMS.35,RMS.41,RMS.43,RMS.53, UFS, i)
                         

proc.time() - pctime






#Salva as bases pnad2008 e RMS --------------------------------------------------------------
# Por seguranca. salVa a base pnad2008. Depois de executados os procedimentos acima,
#Nao eh mais necessario repetilos. Basta carregar a  base a partir daqui

        setwd("C:/Users/rafa/Desktop/Dropbox/R_Ipea/Deslocamento_Ativo") #Define pasta de referencia
        # setwd("R:/Dropbox/R_Ipea/Deslocamento_Ativo") #casa
                       

                       # SalVa a bases para carregar diretamente
                          save(list="pnad2008", file="pnad2008.RData")
                          save(list="RMS", file="RMS.RData")  

                               
                                load("pnad2008.RData")
                                load("RMS.RData")
                       
                      # SalVa a base com pop com 14 anos ou mais de idade e que nao tem dificuldade de locomocao ou compras
                               
                            # exclui da base as pessoas com dificuldade de locomocao e de compras (pq elas sequer respondem a VAR de deslocamento ativo)
                                  pnad2008 <- subset(pnad2008, V1409!=1 | is.na(V1409))
                                  pnad2008 <- subset(pnad2008, V14091!=1 | is.na(V14091))
                                  
                                  RMS <- subset(RMS, V1409!=1 | is.na(V1409))
                                  RMS <- subset(RMS, V14091!=1 | is.na(V14091))

                              # Exclui da base pessoas com menos de 14 anos de idade
                                  pnad200814 <- subset(pnad2008, V8005 > 13)
                                  RMS14 <- subset(RMS, V8005 > 13)
  
                                  save(list="pnad200814", file="pnad200814.RData")
                                  save(list="RMS14", file="RMS14.RData")  
  
                                  load("pnad200814.RData")
                                  load("RMS14.RData")

                       
##############!!!!!! Create survey design object with PNAD design information -----------

#Define pasta de referencia
    setwd("C:/Users/rafa/Desktop/Dropbox/R_Ipea/Deslocamento_Ativo")  


      #Cria desenho da amostra para base pnad2008

          load("pnad200814.RData")
          attach(pnad200814)
          
          pnad200814$urban[pnad200814$V4728<4] <-"Urban"
          pnad200814$urban[pnad200814$V4728>3 & pnad200814$V4728<9] <-"Rural"
          
          
          pnad200814$metro[pnad200814$V4727==1] <-"metro"
          pnad200814$metro[pnad200814$V4727!=1] <-"not metro"
          
          
          
          
          #define como imputar variancia quando houver apenas um domicilio (PSU) no estrato
          options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    
          #Cria objeto de desenho da amostra                      
          sample.pnad08 <- svydesign(
                                    id = ~V4618 , #PSU
                                    strata = ~V4617 , #Strat
                                    data = pnad200814 ,
                                    weights = V4729 , #PesoPessoa
                                    nest = TRUE
                                    )


            #Cria desenho da amostra para base RMS
            
              load("RMS14.RData")
              attach(RMS14)
              RMS14$DecileRM <- as.numeric(as.character(RMS14$DecileRM)) #transforma Var Decile RM em numerica
              attach(RMS14)
              options( survey.lonely.psu = "adjust" )  
              sample.RMS <- svydesign(
                                      id = ~V4618 , #PSU
                                      strata = ~V4617 , #Strat
                                      data = RMS14 ,
                                      weights = V4729 , #PesoPessoa
                                      nest = TRUE
                                      )


gc()
              #Cria desenho da amostra para base pnad2008 Homens e Mulheres
                sample.homens <- subset(sample.pnad08, V0302==2) 
                sample.mulheres <- subset(sample.pnad08, V0302==4)
                

              #Cria desenho da amostra para base RMS Ricos e POBRES [quintil de renda 1 e 5== decil 1+2 e 9+10]
                sample.RMSrich <- subset(sample.RMS, DecileRM>8)
                sample.RMSpoor <- subset(sample.RMS, DecileRM<3)




              #Cria desenho da amostra para base pnad2008 Homens e Mulheres
                sample.homensRM <- subset(sample.RMS, V0302==2) 
                sample.mulheresRM <- subset(sample.RMS, V0302==4)




          #Cria desenho da amostra para base RMS Homens e Mulheres Ricos e Pobres [quintil de renda 1 e 5== decil 1+2 e 9+10]
           #Homens
            sample.menRMSrich <- subset(sample.RMS, DecileRM>8 & V0302==2)
            sample.menRMSpoor <- subset(sample.RMS, DecileRM<3 & V0302==2)

          #Mulheres
            sample.womenRMSrich <- subset(sample.RMS, DecileRM>8 & V0302==4)
            sample.womenRMSpoor <- subset(sample.RMS, DecileRM<3 & V0302==4)



######################!!!!!! RESULTADOS   RESULTADOS  RESULTADOS ###################### 
######################!!!!!! RESULTADOS   RESULTADOS  RESULTADOS ###################### 
######################!!!!!! RESULTADOS   RESULTADOS  RESULTADOS ###################### 



#### Tabelas Geral do Brasil - HOMENS x Mulheres [tabela H.1 a H.7 e H.total, e repete para M.1 ...#### 

    # obs. Rodar os dados duas vezes, uma com cada design: 'sample.mulheres' e 'sample.homens' para obter resultados de cada sexo

  attach(pnad200814)

    # Tabela H.1 - Urbano vs Rural
        svyby(
              ~factor( V1410==2 ) ,
              ~V4728<4 ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
        
 
        
        
        
    # Tabela H.2 - Metropole vs Interior
        svyby(
              ~factor( V1410==2 ) ,
              ~V4727 ==1,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )

    # Tabela H.3 - Regioes
        svyby(
              ~factor( V1410==2 ) ,
              ~regiao ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )


    # Tabela H.4 - Decis de Renda (Grafico 1, trocar sample para ter homens e mulheres)
        svyby(
              ~factor( V1410==2 ) ,
              ~DecileBR ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )


    # Tabela H.5 - Grupos de anos de estudo (Grafico 2, trocar sample para ter homens e mulheres)
        svyby(
              ~factor( V1410==2 ) ,
              ~V4838 ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )


    # Tabela H.6 - Grupos de Idade 
        svyby(
              ~factor( V1410==2 ) ,
              ~AGE ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )


    # Tabela H.7 - Grupos de Idade (Grafico 3, trocar sample para ter homens e mulheres)
        svyby(
              ~factor( V1410==2 ) ,
              ~agegroup ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )


    # Tabela H.Total
        svyciprop(
                  ~factor( V1410==2 ) ,
                  design = sample.mulheres ,
                  vartype="ci",
                  level = 0.95,
                  )

#limpa memoria
gc()

    # Tabela HH.1 - Urbano vs Rural
        svyby(
              ~factor( V1411>3 ) ,
              ~V4728<4 ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
        
    # Tabela HH.2 - Metropole vs Interior
        svyby(
              ~factor( V1411>3 ) ,
              ~V4727==1 ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
        
    # Tabela HH.3 - Regioes
        svyby(
              ~factor( V1411>3 ) ,
              ~regiao ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
        
        
    # Tabela HH.4 - Decis de Renda
        svyby(
              ~factor( V1411>3 ) ,
              ~DecileBR ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
            
        
    # Tabela HH.5 - Grupos de anos de estudo
        svyby(
              ~factor( V1411>3 ) ,
              ~V4838 ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
        
        
    # Tabela HH.6 - Grupos de Idade
        svyby(
              ~factor( V1411>3 ) ,
              ~AGE ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )


    # Tabela HH.7 - Grupos quinquenais de Idade
        svyby(
              ~factor( V1411>3 ) ,
              ~agegroup ,
              design = sample.mulheres ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )      
        

    # Tabela HH.Total
        svyciprop(
                  ~factor( V1411>3 ) ,
                  design = sample.mulheres ,
                  vartype="ci",
                  level = 0.95,
                  )




###### (Grafico 4) Tabela de % Desloc Ativo V1410==2 das RMs ----------
  detach(pnad200814)
  attach(RMS14)

    # RMS.1 - Media de todas RMs
    
        print(svyciprop(~V1410==2, design=sample.RMS, method = c("likelihood"), level = 0.95))     
    
    
    # RMS.2 - Media separada de cada RMS separadamente
    
        svyby(
              ~factor( V1410==2 ) ,
              ~UF ,
              design = sample.RMS ,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
    
    # RMS.3 - Media dos Ricos de todas RMS
    
        print(svyciprop(~V1410==2, design=sample.RMSrich, method = c("likelihood"), level = 0.95))
    
    
    # RMS.4 - Media dos Ricos de cada RMS separadamente
    
        svyby(
              ~factor( V1410==2 ) ,
              ~UF ,
              design = sample.RMSrich,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
    
    
    # RMS.5 - Media dos Pobres de todas RMS
    
       print(svyciprop(~V1410==2, design=sample.RMSpoor, method = c("likelihood"), level = 0.95))
     
    
    # RMS.6 - Media dos Pobres de cada RMS separadamente
    
        svyby(
              ~factor( V1410==2 ) ,
              ~UF ,
              design = sample.RMSpoor,
              vartype="ci",
              level = 0.95,
              svyciprop
              )
    
    
    



############## Conferencia com Knuth et al (2011) - Tabelas para... #-----

attach(pnad200814)

  #Tabela 1 da Knuth et al (2011), descricao do numero de pessoas na base
     
    freq(regiao, V4729, plot=F) #regiao [Meu total==139,917,340, Knuth Total==142.533.480]
                                # todos valores absolutos e propocoes ficam muito proximos, mas nunca batem perfeitamente
                                # Acredito que seja por conta da reponderacao que o IBGE fez após o Censo 2010



  #Tabela 2 da Knuth et al (2011), pratica de transporte ativo 


      #Região [diferencas muito grandes]
        svyby(
              ~factor( V1411>3 ) ,
              ~regiao ,  
              design = sample.pnad08,  
              vartype="ci",  level = 0.95,  svyciprop
              )

      #Sexo
        svyby(
              ~factor( V1411>3 ) ,
              ~V0302 ,  
              design = sample.pnad08,  
              vartype="ci",  level = 0.95,  svyciprop
              )

      #Age
        svyby(
              ~factor( V1411>3 ) ,
              ~AGE ,  
              design = sample.pnad08,  
              vartype="ci",  level = 0.95,  svyciprop
              )





# =========================================================== #
#       Tabela 5A - Compara Homens e Mulheres nas RMs         #       



###### Tabela de % Desloc Ativo V1410==2 das RMs ----------
detach(pnad200814)
attach(RMS14)

# RMS.1 - Media de todas RMs

    print(svyciprop(~V1410==2, design=sample.RMS, method = c("likelihood"), level = 0.95))     


# RMS.2 - Media separada de cada RMS separadamente

    svyby(
          ~factor( V1410==2 ) ,
          ~UF ,
          design = sample.RMS ,
          vartype="ci",
          level = 0.95,
          svyciprop
          )







###### (Grafico 4.men) % de Homens com desloc ativo (V1410==2) por renda nas RMs ----------

# RMS.1 - Media dos Homens de todas RMS
  print(svyciprop(~V1410==2, design=sample.homensRM, method = c("likelihood"), level = 0.95))

# RMS.2 - Media dos Homens de cada RMS separadamente
    svyby(
          ~factor( V1410==2 ) ,
          ~UF ,
          design = sample.homensRM,
          vartype="ci",
          level = 0.95,
          svyciprop
          )

# RMS.3 - Media dos homens Ricos de todas RMS
  print(svyciprop(~V1410==2, design=sample.menRMSrich, method = c("likelihood"), level = 0.95))

# RMS.4 - Media dos Homens Ricos de cada RM separadamente
    svyby(
          ~factor( V1410==2 ) ,
          ~UF ,
          design = sample.menRMSrich,
          vartype="ci",
          level = 0.95,
          svyciprop
          )

# RMS.5 - Media dos Homens Pobres de todas RMS
  print(svyciprop(~V1410==2, design=sample.menRMSpoor, method = c("likelihood"), level = 0.95))

# RMS.6 - Media dos Homens Pobres de cada RMS separadamente
    svyby(
          ~factor( V1410==2 ) ,
          ~UF ,
          design = sample.menRMSpoor,
          vartype="ci",
          level = 0.95,
          svyciprop
          )






###### (Grafico 4.women) % de Mulheres com desloc ativo (V1410==2) por renda nas RMs ----------

# RMS.1 - Media das mulheres de todas RMS
print(svyciprop(~V1410==2, design=sample.mulheresRM, method = c("likelihood"), level = 0.95))

# RMS.2 - Media das mulheres de cada RMS separadamente
    svyby(
          ~factor( V1410==2 ) ,
          ~UF ,
          design = sample.mulheresRM,
          vartype="ci",
          level = 0.95,
          svyciprop
          )

# RMS.3 - Media das mulheres Ricas de todas RMS
    print(svyciprop(~V1410==2, design=sample.womenRMSrich, method = c("likelihood"), level = 0.95))

# RMS.4 - Media das mulheres Ricas de cada RM separadamente
    svyby(
          ~factor( V1410==2 ) ,
          ~UF ,
          design = sample.womenRMSrich,
          vartype="ci",
          level = 0.95,
          svyciprop
          )

# RMS.5 - Media das mulheres Pobres de todas RMS
    print(svyciprop(~V1410==2, design=sample.womenRMSpoor, method = c("likelihood"), level = 0.95))

# RMS.6 - Media das Mulheres Pobres de cada RM separadamente
    svyby(
          ~factor( V1410==2 ) ,
          ~UF ,
          design = sample.womenRMSpoor,
          vartype="ci",
          level = 0.95,
          svyciprop
          )




###### (Grafico 4.men 30min) % de Homens com desloc ativo maior q 30minutos (V1411>3) por renda nas RMs ----------
      
      # RMS.1 - Media dos Homens de todas RMS
      print(svyciprop(~V1411>3, design=sample.homensRM, method = c("likelihood"), level = 0.95))
      
      # RMS.2 - Media dos Homens de cada RMS separadamente
      svyby(
        ~factor( V1411>3 ) ,
        ~UF ,
        design = sample.homensRM,
        vartype="ci",
        level = 0.95,
        svyciprop
        )
      
      # RMS.3 - Media dos homens Ricos de todas RMS
      print(svyciprop(~V1411>3, design=sample.menRMSrich, method = c("likelihood"), level = 0.95))
      
      # RMS.4 - Media dos Homens Ricos de cada RM separadamente
      svyby(
        ~factor( V1411>3 ) ,
        ~UF ,
        design = sample.menRMSrich,
        vartype="ci",
        level = 0.95,
        svyciprop
        )
      
      # RMS.5 - Media dos Homens Pobres de todas RMS
      print(svyciprop(~V1411>3, design=sample.menRMSpoor, method = c("likelihood"), level = 0.95))
      
      # RMS.6 - Media dos Homens Pobres de cada RMS separadamente
      svyby(
        ~factor( V1411>3 ) ,
        ~UF ,
        design = sample.menRMSpoor,
        vartype="ci",
        level = 0.95,
        svyciprop
         )




###### (Grafico 4.women 30min) % de Mulheres com desloc ativo maior q 30minutos(V1411>3) por renda nas RMs ----------
      
      # RMS.1 - Media das mulheres de todas RMS
      print(svyciprop(~V1411>3, design=sample.mulheresRM, method = c("likelihood"), level = 0.95))
      
      # RMS.2 - Media das mulheres de cada RMS separadamente
      svyby(
        ~factor( V1411>3 ) ,
        ~UF ,
        design = sample.mulheresRM,
        vartype="ci",
        level = 0.95,
        svyciprop
        )
      
      # RMS.3 - Media das mulheres Ricas de todas RMS
      print(svyciprop(~V1411>3, design=sample.womenRMSrich, method = c("likelihood"), level = 0.95))
      
      # RMS.4 - Media das mulheres Ricas de cada RM separadamente
      svyby(
        ~factor( V1411>3 ) ,
        ~UF ,
        design = sample.womenRMSrich,
        vartype="ci",
        level = 0.95,
        svyciprop
        )
      
      # RMS.5 - Media das mulheres Pobres de todas RMS
      print(svyciprop(~V1411>3, design=sample.womenRMSpoor, method = c("likelihood"), level = 0.95))
      
      # RMS.6 - Media das Mulheres Pobres de cada RM separadamente
      svyby(
        ~factor( V1411>3 ) ,
        ~UF ,
        design = sample.womenRMSpoor,
        vartype="ci",
        level = 0.95,
        svyciprop
        )









###### (anexo)   Tabela de % Desloc acima de 30 min. V1411>3 nas RMs ----------



# RMS.A - Media de todas RMs

print(svyciprop(~V1411>3, design=sample.RMS, method = c("likelihood"), level = 0.95))     


# RMS.B - Media separada de cada RMS separadamente

svyby(
  ~factor( V1411>3 ) ,
  ~UF ,
  design = sample.RMS ,
  vartype="ci",
  level = 0.95,
  svyciprop
)

# RMS.C - Media dos Ricos de todas RMS

print(svyciprop(~V1411>3, design=sample.RMSrich, method = c("likelihood"), level = 0.95))


# RMS.D - Media dos Ricos de cada RMS separadamente

svyby(
  ~factor( V1411>3 ) ,
  ~UF ,
  design = sample.RMSrich,
  vartype="ci",
  level = 0.95,
  svyciprop
)


# RMS.E - Media dos Pobres de todas RMS

print(svyciprop(~V1411>3, design=sample.RMSpoor, method = c("likelihood"), level = 0.95))


# RMS.F - Media dos Pobres de cada RMS separadamente

svyby(
  ~factor( V1411>3 ) ,
  ~UF ,
  design = sample.RMSpoor,
  vartype="ci",
  level = 0.95,
  svyciprop
)




# ============================================================ fim.


######### Testes Graficos #
crosstab(AGE, V1411, weight= V4729, plot=F)
freq(V1411, V4729, plot=F)




# Teste Graficos

  # Graf1  % de pessoas que vao a peh, por decil de renda
    graf1 <- svyby(
                  ~factor( V1410==2 ) ,
                  ~DecileBR ,
                  design = sample.pnad08 ,
                  vartype="ci",
                  level = 0.95,
                  svyciprop
                  )

#Graf1 eh um dataframe
structure(graf1)
plot(graf1)
str(graf1)

attach(graf1)
plot(graf1[,4],
     type="l", #chart type=line
     lty=4, #Line type
     lwd=2, #espessura da linha
     col="75", #color
     xlim=c(0,10),ylim=c(0,0.7), #escala dos eixos
     xlab="Income Decile", #Label de x
     ylab="%", #Label de y
     main="Prática de deslocamento ativo segundo decil de renda. Brasil,2008" #Chart Title
     )

gc()

# traçando uma reta horizontal no eixo y=0
abline(h=0)

#esse pacote poderia ajudar, mas acho q não 
=> http://gjabel.wordpress.com/2012/08/13/the-fanplot-package-for-r/


#deu certo. http://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph
    
    graflong <- melt(graf1, id="DecileBR")
    
    View(graflong)
    
    qplot(DecileBR, value, data=graflong, color=variable) 




### ver p. 165 do livro ggplot2


#adiciona uma variavel por vez

ggplot(graf1, aes(DecileBR)) +
  geom_line(aes(y = ci_l, colour = "ci_l")) +
  geom_line(aes(y = ci_u, colour = "ci_u")) 



#teste 2 http://stackoverflow.com/questions/12083387/how-to-create-line-chart-with-margins-of-error-in-r


ggplot(graf1, aes(x=DecileBR, y=ci_u, 
                       ymin=ci_l))+
  geom_line(colour="red", size=1.2)+
  geom_point(pch=2)+
  geom_errorbar(width=0.9)


# esse parece que vai. Falta testar
http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
vi isso aqui: http://psybercity.co.uk/2013/12/26/function-to-overlay-histograms/
  
  
  
  
  
  
  
  
  
  
  


# 1 total %
  svyby(
    ~factor( V1410==2 ) ,
    ~ V0302,
    design = sample.pnad08 ,
    vartype="ci",
    level = 0.95,
    svyciprop)

# 1 total abs
svytable(~factor( V1410==2 )+V0302, design = sample.pnad08)


# 2 urban %
svyby(
  ~factor( V1410==2 ) ,
  ~ urban+V0302,
  design = sample.pnad08 ,
  vartype="ci",
  level = 0.95,
  svyciprop)

# 2 urban abs
svytable(~( V1410==2 )+urban+V0302, design = sample.pnad08)



# 3 metro %
svyby(
  ~factor( V1410==2 ) ,
  ~ metro+V0302,
  design = sample.pnad08 ,
  vartype="ci",
  level = 0.95,
  svyciprop)

# 3 metro abs
svytable(~( V1410==2 )+metro+V0302, design = sample.pnad08)


# 4 region %
svyby(
  ~factor( V1410==2 ) ,
  ~ regiao+V0302,
  design = sample.pnad08 ,
  vartype="ci",
  level = 0.95,
  svyciprop)

# 4 region abs
svytable(~( V1410==2 )+regiao+V0302, design = sample.pnad08)



### Tabela 2 final para publicao, 13 out 2015

svytable(~Vcount, design = sample.pnad08)

#Sex
svytable(~V0302, design = sample.pnad08)
svyciprop(~V0302==2, design = sample.pnad08, vartype="ci",level = 0.95)
svyciprop(~V0302==4, design = sample.pnad08, vartype="ci",level = 0.95)

#urban
svytable(~urban, design = sample.pnad08)
svyciprop(~urban=="Rural", design = sample.pnad08, vartype="ci",level = 0.95)
svyciprop(~urban=="Urban", design = sample.pnad08, vartype="ci",level = 0.95)

#metro
svytable(~metro, design = sample.pnad08)
svyciprop(~metro=="not metro", design = sample.pnad08, vartype="ci",level = 0.95)
svyciprop(~metro=="metro", design = sample.pnad08, vartype="ci",level = 0.95)

#regiao
svytable(~regiao, design = sample.pnad08)
svyciprop(~regiao=="Centro Oeste", design = sample.pnad08, vartype="ci",level = 0.95)
svyciprop(~regiao=="Nordeste", design = sample.pnad08, vartype="ci",level = 0.95)
svyciprop(~regiao=="Norte", design = sample.pnad08, vartype="ci",level = 0.95)
svyciprop(~regiao=="Sudeste", design = sample.pnad08, vartype="ci",level = 0.95)
svyciprop(~regiao=="Sul", design = sample.pnad08, vartype="ci",level = 0.95)


table(pnad200814$regiao)
