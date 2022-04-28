library(sf)
library(geobr)
library(mapview)
library(magrittr)
library(furrr)
library(data.table)
library(dplyr)
library(readr)

mapview::mapviewOptions(platform = 'leafgl')


df <- read_rds('pns2013pes.rds')
head(df)

table(df$Q002)

setDT(df)
df[, V00291 := as.numeric(V00291)]
df[, sum(V00291), by=Q002]


df_det <- df[M001 ==1 ,]

df_det[, sum(V00291), by=Q002]




