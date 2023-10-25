a <- 3
b <- 6
A <- 3
B <- 4
C <- A+B
c <- a+b
C
c

ls()
rm(A)
ls()

#create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")

#just for fun, create some more, this time some normally distributed
#vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")

?plot

df <- data.frame(Data1, Data2)
plot(df, col="green")

library(tidyverse)
#show the first 10 and then last 10 rows of data in df...
df %>%
  head(n = 10L)
df %>%
  tail(n = 10L)

df[1:10, 1]
df[5:15,]
df[c(2,3,6),2]
df[,1]

library(dplyr)
df <- df %>%
  dplyr::rename(column1 = Data1, column2 = Data2)

#df <- df %>%
  #dplyr::rename(c_1 = column1, c_2 = column2)

df <- df %>%
  dplyr::rename(column1 = c_1, column2 = c_2)

df %>% 
  dplyr::select(column1)

df$column1
df[["column1"]]

LondonDataOSK<- read.csv("LondonData.csv", 
                         sep = ",")

LondonDataOSK<- read.csv("LondonData.csv", 
                         header = TRUE, sep = ",", encoding = "latin1")

install.packages("here")

library(here)
here::here()

LondonDataOSK<- read.csv(here::here("LondonData.csv"), 
                         header = TRUE, sep = ",",  
                         encoding = "latin1")

#wrang the data in straight from the web using read_csv, 
#skipping over the 'n/a' entries as you go...
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")

class(LondonData)

# or, if you have your old skool data
class(LondonDataOSK)

Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                       locale = locale(encoding = "latin1"))

Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist #read in as something other than numeric?

LondonData <- edit(LondonData)

summary(df)

LondonData %>%
  colnames() %>%
  # just look at the head, top5
  head()

LondonBoroughs<-LondonData[626:658,]

LondonBoroughs<-LondonData %>%
  slice(626:658)

Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)
Femalelifeexp

LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))

LondonBoroughs$`Ward name`

LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()

LondonBoroughs<-LondonBoroughs %>%
  distinct()
LondonBoroughs$`Ward name`

#select columns 1,19,20 and 21
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]
LondonBoroughs_manualcols

#select columns 1,19,20 and 21
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))
LondonBoroughs_dplyrcols

LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name"))
LondonBoroughs_contains

install.packages("janitor")
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`) %>%
  clean_names()

Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2) %>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy)) %>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

#top of data
slice_head(Life_expectancy, n=5)

#bottom of data
slice_tail(Life_expectancy,n=5)

Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2

Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare) %>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group

Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  mutate(across(UKdiff, round, 0)) %>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" "))) %>%
  group_by(UKcompare) %>%
  summarise(count=n())

Life_expectancy3

Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  mutate(across(is.numeric, round, 3)) %>%
  mutate(across(UKdiff, round, 0))

plot(LondonBoroughs$male_life_expectancy_2009_13,
         LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)

install.packages("plotly")

library(plotly)

plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")

install.packages("maptools")

install.packages(c("classInt", "tmap"))

# might also need these ones
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))

