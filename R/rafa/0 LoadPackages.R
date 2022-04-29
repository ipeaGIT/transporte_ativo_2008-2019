


#####################  List of packages -------------------------------------------------------


# List of packages we are going to use
  list.of.packages <- 
        c(
          # download data
          "downloader"
          
          # Read data 
          , "readr"
          , "foreign"
          , "readxl"
          , "SAScii"
          
          # Manipulate data frames
          , "data.table"
          , "dplyr"
          , "fasttime"
          , "pbapply"
          , "tidyr"
          , "reshape"
          , "plyr"
          , "taRifx"
          , "sqldf"
          
          # Complex Sample Survey analysis
          , "survey"
          , "descr"
          
          # stats functions
          , "ineq"
          , "stats"
          
          # Make Plots
          , "ggplot2"
          , "grid"
          , "gridExtra"
          , "cowplot"
          
          # Plots: Nice themes and colors
          , "RColorBrewer"
          , "scales"
          , "ggthemes"
          , "viridis"
          , "gtable"
          , "wesanderson"
          
          # Miscellaneous 
          , "beepr"
          , "bit64"
        )

#####################  install packages -------------------------------------------------------
# install packages that are NOT already installed
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)


#####################  Load libraries  -------------------------------------------------------
lapply(list.of.packages, require, character.only = TRUE)

# Clean environment and memory
rm(list=ls())
gc(reset=T )








# # download data
# library(downloader) #Download files
# 
# 
# # Read data 
# library(readr) # fast read of fixed width files
# library(foreign)    # to export DF to other formats
# library(readxl)     # read excel docs
# library(rJaVa) #para ler e ciar arquiVos em Excel - pede xlsX
# library(SAScii)
# 
# 
# # Manipulate data frames
# library(data.table) # to manipulate data frames (read.csv is ultrafast for reading CSV files)
# library(dplyr)      # to manipulate data frames
# library(fasttime)   # Fast version of as.POSIXct.character for GMT fixed format
# library(pbapply)    # to include progress bar in apply
# library(tidyr) # Ro reshape data.frames (e.g. 'gather')
# library(reshape)
# library(plyr) 
# library(taRifx)
# library(sqldf)
# 
# 
# # Complex Sample Survey analysis
# library(survey) #complex sample surVeys
# library(descr)  # para fazer tabela de freq com expansao da amostra
# 
# 
# # stats functions
# library(ineq)
# library(stats)
# # gstat #geostatistical model
# # spatstat #spatial point patter analysis
# 
# 
# # Make Plots
# library(ggplot2)    # to make charts and maps
# library(grid) #to create nice ggplots - fte_theme
# library(gridExtra)  # Arrange Grid of ggplots
# library(cowplot)    # arrange ggplots - http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
# # library(bigvis) # ggplot para big data
# 
# 
# # Plots: Nice themes and colors
# library(RColorBrewer)
# library(scales)     # to get color scales
# library(ggthemes) #themes for ggplot charts
# library(viridis)    # nice color pallet
# library(gtable)
# library(wesanderson) # colors pallets https://github.com/karthik/wesanderson#wes-anderson-palettes
# 
# 
# # Miscellaneous 
# library(beepr)      # Beeps at the end of the command
# options(digits=4)   # number of digits to show
# options(scipen=999) # disable scientific notation
# library(bit64)

