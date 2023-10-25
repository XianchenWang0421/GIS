library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)
#read in the shapefile

shape <- st_read("D:/CASA/CASA_0005/Week_1/New Zealand/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")

Datatypelist <- shape %>% 
  summarise_all(class)
Datatypelist

summary(shape)
shape
# read in the csv
mycsv <- read_csv("D:/CASA/CASA_0005/Week_1/New Zealand/employee.csv") 
mycsv
# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="TA2018_V1_", 
        by.y="Territorial authority code")
shape%>%
  head(., n=5)
# set tmap to plot
tmap_mode("plot")
# have a look at the map
#qtm(shape, fill = "Paid employee")
map <- tm_shape(shape) +
  tm_fill(col = "Paid employee", style = "quantile", palette = "Reds")
tmap_save(map, "D:/CASA/CASA_0005/Week_1/New Zealand/R_map.jpeg")
# write to a .gpkg
shape %>%
  st_write(.,"D:/CASA/CASA_0005/Week_1/New Zealand/R_homework_1.gpkg",
           "paid_employee",
           delete_layer=TRUE)
# connect to the .gpkg
con <- dbConnect(SQLite(),dbname="D:/CASA/CASA_0005/Week_1/New Zealand/R_homework_1.gpkg")
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