#Load Packages (ignore any error messages about being built under a 
#different R version):
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)

# this will take a few minutes
# EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")

# this will take a few minutes
# geojson in local folder
# EW <- st_read(here::here("prac2_data",
#                         "Local_Authority_Districts__December_2015__Boundaries.geojson"))

# shapefile in local folder
EW <- st_read(here::here("Local_Authority_Districts",
                         "LAD_DEC_2022_UK_BFC_V2.shp"))

LondonMap <- EW %>%
  filter(str_detect(LAD22CD, "^E09"))

LondonMap <- EW %>%
  filter(str_detect(LAD22CD, "E09"))

#plot it using the qtm function
qtm(LondonMap)

LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names() %>%
  # the . here just means use the data already loaded
  filter(str_detect(lad22cd, "^E09")) %>%
  merge(.,
        LondonData, 
        by.x="lad22cd", 
        by.y="new_code",
        no.dups = TRUE) %>%
  distinct(.,lad22cd,
           .keep_all = TRUE)

BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad22cd, "^E09")) %>%
  left_join(., 
            LondonData,
            by = c("lad22cd" = "new_code")) %>%
  distinct(.,lad22cd,
           .keep_all = TRUE)

library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

install.packages("OpenStreetMap")

tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>%
  tmaptools::read_osm(., type = "osm", zoom = NULL)

tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))

install.packages("shiny")
install.packages("shinyjs")

palette_explorer()

Life_expectancy4map <- EW %>%
  inner_join(., 
             Life_expectancy4,
             by = c("LAD22CD" = "new_code"))%>%
  distinct(.,LAD22CD, 
           .keep_all = TRUE)

tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UKdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))

flytipping1 <- read_csv("D:/CASA/CASA_0005/Week_1/London/fly-tipping-borough.csv", 
                            col_types = cols(
                              code = col_character(),
                              area = col_character(),
                              year = col_character(),
                              total_incidents = col_number(),
                              total_action_taken = col_number(),
                              warning_letters = col_number(),
                              fixed_penalty_notices = col_number(),
                              statutory_notices = col_number(),
                              formal_cautions = col_number(),
                              injunctions = col_number(),
                              prosecutions = col_number()
                            ))
# view the data
view(flytipping1)

#convert the tibble into a tidy tibble
flytipping_long <- flytipping1 %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )

# view the data
view(flytipping_long)

#an alternative which just pulls everything out into a single table
flytipping2 <- flytipping1[,1:4]

view(flytipping2)

#pivot the tidy tibble into one that is suitable for mapping
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )

view(flytipping_wide)

widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)

view(widefly)

BoroughDataMap_widefly <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad22cd, "^E09")) %>%
  left_join(., 
            widefly,
            by = c("lad22cd" = "code")) %>%
  distinct(.,lad22cd,
           .keep_all = TRUE)
qtm(BoroughDataMap_widefly, fill="2012-13")

#shape_simple = st_simplify("")
