# This script downloads PNS survey data of 2013 from IBGE website and saves it to your local computer
# Script written by Rafael Pereira (urbandemographics.blogspot.com) and modified 
# by Joao Pedro Bazzo
# Jun. 2020, Brasilia


# 1:Download Pns2013 DATA----------------

dir.create("./data-raw/")
dir.create("./data-raw/PNS")
dir.create("./data-raw/PNAD")

download.file(url = "https://ftp.ibge.gov.br/PNS/2013/Microdados/Dados/PNS_2013.zip",
              destfile = "./data-raw/PNS/pns_2013.zip")

download.file(url = "https://ftp.ibge.gov.br/PNS/2019/Microdados/Dados/PNS_2019_20210826.zip",
              destfile = "./data-raw/PNS/pns_2019.zip")

download.file(url = "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/reponderacao_2001_2009/PNAD_reponderado_2008.zip",
              destfile = "./data-raw/PNAD/pnad_2008.zip")

unzip(zipfile = "./data-raw/PNS/pns_2013.zip",
      exdir = "./data-raw/PNS/pns_2013/")

unzip(zipfile = "./data-raw/PNS/pns_2019.zip",
      exdir = "./data-raw/PNS/pns_2019/")

unzip(zipfile = "./data-raw/PNAD/pnad_2008.zip",
      exdir = "./data-raw/PNAD/")
