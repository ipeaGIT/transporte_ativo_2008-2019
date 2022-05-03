dir.create("../../data-raw/PNS")
dir.create("../../data-raw/PNAD")
dir.create("../../data-raw/PNS/2013/")
dir.create("../../data-raw/PNS/2019/")

# PNS 2013----
download.file(url = "https://ftp.ibge.gov.br/PNS/2013/Microdados/Dados/PNS_2013.zip",
              destfile = "../../data-raw/PNS/2013/pns_2013.zip",mode = "wb")

download.file(url = "https://ftp.ibge.gov.br/PNS/2013/Microdados/Documentacao/Chaves_PNS_2013.pdf",
              destfile = "../../data-raw/PNS/2013/Chaves_PNS_2013.pdf",mode = "wb")

download.file(url = "https://ftp.ibge.gov.br/PNS/2013/Microdados/Documentacao/Plano_Amostral_PNS_2013.pdf",
              destfile = "../../data-raw/PNS/2013/Plano_Amostral_PNS_2013.pdf",mode = "wb")

download.file(url = "https://ftp.ibge.gov.br/PNS/2013/Microdados/Documentacao/Questionario_PNS_2013.pdf",
              destfile = "../../data-raw/PNS/2013/Questionario_PNS_2013.pdf",mode = "wb")

download.file(url = "https://ftp.ibge.gov.br/PNS/2013/Microdados/Documentacao/Dicionario_e_input_20200930.zip",
              destfile = "../../data-raw/PNS/2013/Dicionario_e_input_2013.zip",mode = "wb")

# PNS 2019 -----

download.file(url = "https://ftp.ibge.gov.br/PNS/2019/Microdados/Dados/PNS_2019_20210826.zip",
              destfile = "../../data-raw/PNS/2019/pns_2019.zip",mode = "wb")

download.file(url = "https://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/Chaves_PNS_2019.pdf",
              destfile = "../../data-raw/PNS/2019/Chaves_PNS_2019.pdf",mode = "wb")

download.file(url = "https://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/Dicionario_e_input_20220114.zip",
              destfile = "../../data-raw/PNS/2019/Dicionario_e_input_2019.zip",mode = "wb")

# PNAD 2008 ----

download.file(url = "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/reponderacao_2001_2009/PNAD_reponderado_2008.zip",
              destfile = "../../data-raw/PNAD/pnad_2008.zip",mode = "wb")

# Extract-----
# pns 2013
unzip(zipfile = "../../data-raw/PNS/2013/pns_2013.zip",
      exdir = "../../data-raw/PNS/2013")
unzip(zipfile = "../../data-raw/PNS/2013/Dicionario_e_input_2013.zip",
      exdir = "../../data-raw/PNS/2013")


# pns 2019
unzip(zipfile = "../../data-raw/PNS/2019/pns_2019.zip",
      exdir = "../../data-raw/PNS/2019")
unzip(zipfile = "../../data-raw/PNS/2019/Dicionario_e_input_2019.zip",
      exdir = "../../data-raw/PNS/2019")

# pnad
unzip(zipfile = "../../data-raw/PNAD/pnad_2008.zip",
      exdir = "../../data-raw/PNAD")
