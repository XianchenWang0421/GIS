library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)
#read in the shapefile

shape <- st_read("D:/CASA/CASA_0005/Week_1/London/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
# read in the csv
mycsv <- read_csv("D:/CASA/CASA_0005/Week_1/London/fly-tipping-borough-1.csv")  
# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="Row Labels")
# set tmap to plot
tmap_mode("plot")
# have a look at the map
qtm(shape, fill = "2012-13")
# write to a .gpkg
shape %>%
  st_write(.,"D:/CASA/CASA_0005/Week_1/London/Rwk1.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)
# connect to the .gpkg
con <- dbConnect(SQLite(),dbname="D:/CASA/CASA_0005/Week_1/London/Rwk1.gpkg")
# list what is in it
con %>%
  dbListTables()
# add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)
# disconnect from it
con %>% 
  dbDisconnect()